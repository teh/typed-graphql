{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, TypeInType #-}
{-# LANGUAGE InstanceSigs, RankNTypes, TypeFamilyDependencies #-}


module Data.GraphQl.Combinators where

import qualified Data.Aeson as Aeson
import Data.Attoparsec.Text (parseOnly, endOfInput)
import qualified Data.GraphQL.AST as AST
import Data.GraphQL.Parser (document)
import Data.Kind (Type)
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)


data Object (name :: Symbol) (returnType :: Type)
data Argument (name :: Symbol) (type_ :: Type)


-- Why have this funky operator? It's meant to be resemble monoid
-- composition. We can't use list prepend (':) because that changes
-- the kind to [Type] and then we can't unpack in the resolve handler.
data a :<> b = a :<> b
infixl 9 :<>

data a :> b = a :> b -- arguments
infixr 8 :>

data a :^^ b = a :^^ b -- nested objects each get the parent as first argument
infixl 8 :^^


-- Build a resolver function with a handler - this resolves a *single*
-- Definition.
class MakeResolver a where
  type Handler a :: Type
  makeResolver :: forall m. Monad m => Handler a -> (AST.Definition -> m Aeson.Value)


instance forall s return. (KnownSymbol s, Aeson.ToJSON return) => MakeResolver (Object s return) where
  type Handler (Object s return) = return
  makeResolver handler = eval
    where
      eval query = undefined


instance forall s rhs return. (KnownSymbol s, Aeson.ToJSON return, MakeResolver rhs) => MakeResolver ((Object s return) :^^ rhs) where
  type Handler ((Object s return) :^^ rhs) = return :^^ (return -> Handler rhs)
  makeResolver handler = eval
    where
      eval query = undefined


instance forall lhs rhs. (MakeResolver lhs, MakeResolver rhs) => MakeResolver (lhs :<> rhs) where
  type Handler (lhs :<> rhs) = (Handler lhs) :<> (Handler rhs)
  makeResolver handler = eval
    where
      eval query = undefined


parseQuery :: Text -> Either String AST.Document
parseQuery s = parseOnly (document <* endOfInput) s

-- let Right (AST.Document [AST.DefinitionOperation (AST.Query (AST.Node _ _ _ [x]))]) = parseOnly (document <* endOfInput) "{ plus }"
