-- Andrés Collares (4.867.464-5)

import Control.Monad.Reader
import Data.Map

-- | Repositorio
type Repo = (Users, Lib)

-- | Registro de usuarios
type Users = Map UID (ULevel, PWD)

type UID = String

data ULevel = Limited | Advanced
  deriving (Show)

type PWD = String

-- | Biblioteca de documentos
type Lib = Map DID (DLevel, Doc)

type DID = String

data DLevel = Public | Confidential
  deriving (Show)

type Doc = String

-- | Monada reader
type MonadRepo a = Reader Repo a

-- | Autenticacion de usuario
accessControl :: UID -> PWD -> MonadRepo (Maybe ULevel)
accessControl uid pwd = do
  (users, lib) <- ask
  case Data.Map.lookup uid users of
    Nothing -> return Nothing
    Just (level, pwd_repo) -> if pwd == pwd_repo then return (Just level) else return Nothing

-- | Chequeo de permiso de acceso a documento
authUserDoc :: UID -> PWD -> DID -> MonadRepo Bool
authUserDoc uid pwd did = do
  control <- accessControl uid pwd
  case control of
    Nothing -> return False
    (Just level) -> do
      (_, lib) <- ask
      case Data.Map.lookup did lib of
        Nothing -> return False
        (Just (dlevel, _)) -> return $ autorizado level dlevel

autorizado :: ULevel -> DLevel -> Bool
autorizado Advanced _ = True
autorizado _ Public = True
autorizado Limited Confidential = False

-- | Listado de DIDs de los documentos que un usuario puede acceder
getUserDocs :: UID -> PWD -> MonadRepo (Maybe [DID])
getUserDocs uid pwd = do
  control <- accessControl uid pwd
  case control of
    Nothing -> return Nothing
    (Just level) -> do
      (_, lib) <- ask
      let user_docs = foldrWithKey (\key (dlevel, _) -> if autorizado level dlevel then (:) key else ([] ++)) [] lib
       in return (Just user_docs)

-- | Repositorio ejemplo
repo = (users, lib)

users :: Users
users =
  fromList
    [ ("U1", (Limited, "u1pwd")),
      ("U2", (Advanced, "u2pwd"))
    ]

lib :: Lib
lib =
  fromList
    [ ("D1", (Public, "Documento Publico")),
      ("D2", (Confidential, "Documento Confidencial"))
    ]

-- | Ejemplos de uso
e1 = runReader (accessControl "U1" "u1pwd") repo

-- Just Limited

e2 = runReader (accessControl "U2" "u1pwd") repo

-- Nothing

e3 = runReader (accessControl "U3" "u1pwd") repo

-- Nothing

e4 = runReader (authUserDoc "U1" "u1pwd" "D1") repo

-- True

e5 = runReader (authUserDoc "U1" "u1pwd" "D2") repo

-- False

e6 = runReader (authUserDoc "U1" "u2pwd" "D1") repo

-- False

e7 = runReader (getUserDocs "U1" "u1pwd") repo

-- Just ["D1"]

e8 = runReader (getUserDocs "U2" "u2pwd") repo

-- Just ["D1","D2"]

e9 = runReader (getUserDocs "U2" "u1pwd") repo

-- Nothing
