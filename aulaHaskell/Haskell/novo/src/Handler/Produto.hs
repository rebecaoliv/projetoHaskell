{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Produto where

import Import
import Handler.Auxiliar

formProduto :: Maybe Produto -> Form Produto
formProduto mp = renderDivs $ Produto
    <$> areq textField      "Nome do produto:" (fmap produtoNome mp)
    <*> areq textField      "Marca do produto:"  (fmap produtoMarca mp)
    <*> areq doubleField    "Preço: " (fmap produtoPreco mp)
    <*> areq textareaField  "Descrição: " (fmap produtoDescr mp)

-- /produto ProdutoR GET POST

getProdutoR :: Handler Html
getProdutoR = do
  (widget,_) <- generateFormPost (formProduto Nothing)
  msg <- getMessage
  defaultLayout (formWidget widget msg ProdutoR "Cadastrar")

postProdutoR :: Handler Html
postProdutoR = do
    ((result,_),_) <- runFormPost (formProduto Nothing)
    case result of
        FormSuccess produto -> do
            runDB $ insert produto
            setMessage [shamlet|
                <div>
                    Produto cadastrado com sucesso
            |]
            redirect ProdutoR
        _ -> redirect HomeR

-- /produtos               ListaProdR GET
-- /catalogo/#ProdutoId    CatalogoR  GET
-- /produto/#ProdutoId/apagar  ApagarProdR POST

-- select * from produto where id = pid
getCatalogoProdR :: ProdutoId -> Handler Html
getCatalogoProdR pid = do
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
    redirect ListarProdR

--select * from produto order by nome;
getListarProdR :: Handler Html
getListarProdR = do
    produtos <- runDB $ selectList [] [Asc ProdutoNome]
    defaultLayout $(whamletFile "templates/listar.hamlet") 

getEditarProdR :: ProdutoId -> Handler Html
getEditarProdR pid = do 
    produto <- runDB $ get404 pid
    (widget,_) <- generateFormPost (formProduto (Just produto))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarProdR pid) "Editar")

postEditarProdR :: ProdutoId -> Handler Html
postEditarProdR pid = do
    _ <- runDB $ get404 pid 
    ((result,_),_) <- runFormPost (formProduto Nothing)
    case result of 
      FormSuccess novoProduto -> do
          runDB $ replace pid novoProduto
          redirect ListarProdR
      _ -> redirect HomeR