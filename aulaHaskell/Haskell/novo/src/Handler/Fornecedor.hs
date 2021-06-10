{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Fornecedor where

import Import
import Handler.Auxiliar

formFornecedor :: Maybe Fornecedor -> Form Fornecedor
formFornecedor mf = renderDivs $ Fornecedor
    <$> areq textField      "Nome do Fornecedor:" (fmap fornecedorNome mf)
    <*> areq textField      "Cnpj:"  (fmap fornecedorCnpj mf)
    <*> areq textField      "Endereço:"  (fmap fornecedorEndereco mf)
    <*> areq textField      "Telefone:" (fmap fornecedorTelefone mf)
    <*> areq textField      "Produto fornecido:" (fmap fornecedorProdutoFornecido mf)

-- /Fornecedor FornecedorR GET POST

getFornecedorR :: Handler Html
getFornecedorR = do
  (widget,_) <- generateFormPost (formFornecedor Nothing)
  msg <- getMessage
  defaultLayout (formWidget widget msg FornecedorR "Cadastrar")

postFornecedorR :: Handler Html
postFornecedorR = do
    ((result,_),_) <- runFormPost (formFornecedor Nothing)
    case result of
        FormSuccess fornecedor -> do
            runDB $ insert fornecedor
            setMessage [shamlet|
                <div>
                    Fornecedor cadastrado com sucesso
            |]
            redirect FornecedorR
        _ -> redirect HomeR

-- /Fornecedors               ListaFornR GET
-- /catalogo/#FornecedorId    CatalogoR  GET
-- /Fornecedor/#FornecedorId/apagar  ApagarFornR POST

-- select * from Fornecedor where id = fid
getCatalogoFornR :: FornecedorId -> Handler Html
getCatalogoFornR fid = do
    fornecedor <- runDB $ get404 fid
    defaultLayout [whamlet|
        <h1>
            Nome: #{fornecedorNome fornecedor}
        <h1>
            Cnpj: #{fornecedorCnpj fornecedor}
        <h2>
            Endereco: #{fornecedorEndereco fornecedor}
        <h3>
            Telefone: #{fornecedorTelefone fornecedor}
        <h3>
            Produto fornecido: #{fornecedorProdutoFornecido fornecedor}
    |] 

--delete from Fornruto where id = fid
postApagarFornR :: FornecedorId -> Handler Html
postApagarFornR fid = do
    runDB $ delete fid
    redirect ListarFornR

--select * from Fornecedor order by nome;
getListarFornR :: Handler Html
getListarFornR = do
    fornecedores <- runDB $ selectList [] [Asc FornecedorNome]
    defaultLayout $(whamletFile "templates/listarForn.hamlet") 

getEditarFornR :: FornecedorId -> Handler Html
getEditarFornR fid = do 
    fornecedor <- runDB $ get404 fid
    (widget,_) <- generateFormPost (formFornecedor (Just fornecedor))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarFornR fid) "Editar")

postEditarFornR :: FornecedorId -> Handler Html
postEditarFornR fid = do
    _ <- runDB $ get404 fid 
    ((result,_),_) <- runFormPost (formFornecedor Nothing)
    case result of 
      FormSuccess novoFornecedor -> do
          runDB $ replace fid novoFornecedor
          redirect ListarFornR
      _ -> redirect HomeR