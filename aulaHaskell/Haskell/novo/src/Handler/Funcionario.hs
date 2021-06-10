{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Funcionario where

import Import
import Handler.Auxiliar

formFuncionario :: Maybe Funcionario -> Form Funcionario
formFuncionario mfu = renderDivs $ Funcionario
    <$> areq textField      "Nome do Funcionario:" (fmap funcionarioNome mfu)
    <*> areq textField      "Cpf:"  (fmap funcionarioCpf mfu)
    <*> areq textField      "Endereço:"  (fmap funcionarioEndereco mfu)
    <*> areq textField      "Telefone:" (fmap funcionarioTelefone mfu)

-- /Fornecedor FornecedorR GET POST

getFuncionarioR :: Handler Html
getFuncionarioR = do
  (widget,_) <- generateFormPost (formFuncionario Nothing)
  msg <- getMessage
  defaultLayout (formWidget widget msg FuncionarioR "Cadastrar")

postFuncionarioR :: Handler Html
postFuncionarioR = do
    ((result,_),_) <- runFormPost (formFuncionario Nothing)
    case result of
        FormSuccess funcionario -> do
            runDB $ insert funcionario
            setMessage [shamlet|
                <div>
                    Funcionario cadastrado com sucesso
            |]
            redirect FuncionarioR
        _ -> redirect HomeR

-- /Fornecedors               ListaFornR GET
-- /catalogo/#FornecedorId    CatalogoR  GET
-- /Fornecedor/#FornecedorId/apagar  ApagarFornR POST

-- select * from Fornecedor where id = fuid
getCatalogoFuncR :: FuncionarioId -> Handler Html
getCatalogoFuncR fuid = do
    funcionario <- runDB $ get404 fuid
    defaultLayout [whamlet|
        <h1>
            Nome: #{funcionarioNome funcionario}
        <h1>
            Cnpj: #{funcionarioCpf funcionario}
        <h2>
            Endereco: #{funcionarioEndereco funcionario}
        <h3>
            Telefone: #{funcionarioTelefone funcionario}
    |] 

--delete from Fornruto where id = fuid
postApagarFuncR :: FuncionarioId -> Handler Html
postApagarFuncR fuid = do
    runDB $ delete fuid
    redirect ListarFuncR

--select * from Fornecedor order by nome;
getListarFuncR :: Handler Html
getListarFuncR = do
    funcionarios <- runDB $ selectList [] [Asc FuncionarioNome]
    defaultLayout $(whamletFile "templates/listaFunc.hamlet") 

getEditarFuncR :: FuncionarioId -> Handler Html
getEditarFuncR fuid = do 
    funcionario <- runDB $ get404 fuid
    (widget,_) <- generateFormPost (formFuncionario (Just funcionario))
    msg <- getMessage
    defaultLayout (formWidget widget msg (EditarFuncR fuid) "Editar")

postEditarFuncR :: FuncionarioId -> Handler Html
postEditarFuncR fuid = do
    _ <- runDB $ get404 fuid 
    ((result,_),_) <- runFormPost (formFuncionario Nothing)
    case result of 
      FormSuccess novoFuncionario -> do
          runDB $ replace fuid novoFuncionario
          redirect ListarFuncR
      _ -> redirect HomeR