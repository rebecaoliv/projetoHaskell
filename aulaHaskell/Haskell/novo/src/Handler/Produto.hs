{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Produto where

import Import

formProduto :: Form Produto
formProduto = renderDivs $ Produto
    <$> areq textField      (FieldSettings ""
                                            (Just "Nome do produto")
                                            (Just "n1")
                                            Nothing
                                            [("class","formName")]
                            ) Nothing
    <*> areq textField      "Marca do produto:"  Nothing
    <*> areq doubleField    "Preço: " Nothing
    <*> areq textareaField  "Descrição: " Nothing

-- /produto ProdutoR GET POST

getProdutoR :: Handler Html
getProdutoR = do
    (widget,_) <- generateFormPost formProduto
    msg <- getMessage -- Handler (Maybe Text)
    defaultLayout $ [whamlet|
        $maybe mensa <- msg
            <h2>
                ^{mensa}  

        <form action=@{ProdutoR} method=post>
            ^{widget}
            <input type="submit" value="Cadastrar">
    |]

postProdutoR :: Handler Html
postProdutoR = do
    ((result,_),_) <- runFormPost formProduto
    case result of
        FormSuccess produto -> do
            runDB $ insert produto
            setMessage [shamlet|
                <div>
                    PRODUTO INSERIDO COM SUCESSO
            |]
            redirect ProdutoR
        _ -> redirect HomeR

-- /produtos               ListaProdR GET
-- /catalogo/#ProdutoId    CatalogoR  GET
-- /produto/#ProdutoId/apagar  ApagarProdR POST

-- select * from produto where id = pid
getCatalogoR :: ProdutoId -> Handler Html
getCatalogoR pid = do
    produto <- runDB $ get404 pid
    defaultLayout [whamlet|
        <h1>
            Nome: #{produtoNome produto}
        <h1>
          Marca: #{produtoMarca produto}

        <h2>
            Preco: #{produtoPreco produto}
            
        <h3>
            Descr: #{produtoDescr produto}
    |] 

--delete from prodruto where id = pid
postApagarProdR :: ProdutoId -> Handler Html
postApagarProdR pid = do
    runDB $ delete pid
    redirect ListaProdR

--select * from produto order by nome;
getListaProdR :: Handler Html
getListaProdR = do
    -- [Entity 1 (Produto "Caneta" 2.20 ".."), Entity 2 (Produto "Lapis" 1.70 ".."),....]
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    defaultLayout $(whamletFile "templates/listar.hamlet") 