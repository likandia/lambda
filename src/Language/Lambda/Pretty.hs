----------------------------------------
-- |
-- Module : Language.Lambda.Pretty
-- License: MIT
--
-- This module implements a function to print lambda expressions in a more 
-- compact format.
--
-- Example usage:
--
-- > putStrLn (prettify (Application (Term "x") (Abstraction "z" (Application 
-- >                                 (Term "y") (Abstraction "u" (Term "w"))))))
-- >     -- gives x \z.y \u.w
----------------------------------------

module Language.Lambda.Pretty (
  -- * Prettification function
  prettify,
  -- * Constructors
  Expression(..),
  Identifier
      ) where

-- | An @Identifier@ is just an alias for a @String@.

type Identifier = String

-- | The different types of expressions in lambda calculus.

data Expression
  -- | An @Application@ applies one @Expression@ to another.
  = Application Expression Expression
  -- | An @Abstraction@ is a function that takes a named argument (the 
  -- @Identifier@) and has an @Expression@ body.
  | Abstraction Identifier Expression
  -- | A @Term@ is a named thing that can be equated with another @Term@.
  | Term Identifier deriving (Eq, Show, Read)

-- | Print the Application without parentheses for both argument terms.

printn :: Expression -> String
printn (Application x y) = (prettify x) ++ " " ++ (prettify y)
printn _ = undefined

-- | Print the Application with parentheses for both argument terms.

printb :: Expression -> String
printb (Application x y) = "(" ++ (prettify x) ++ ") (" ++ (prettify y)
                                    ++ ")"
printb _ = undefined

-- | Print the Application with parentheses for only the left argument term.

printl :: Expression -> String
printl (Application x y) = "(" ++ (prettify x) ++ ") "
                                           ++ (prettify y)
printl _ = undefined

-- | Print the Application with parentheses for only the right argument term.

printr :: Expression -> String
printr (Application x y) = (prettify x) ++ " (" ++ (prettify y)
                                           ++ ")"
printr _ = undefined

-- | Takes a lambda expression specified in words and converts it to a symbolic 
-- format, with as few parentheses as possible.

prettify :: Expression -> String

prettify x =
  case x of
   Term y -> y
   y @(Application (Abstraction _ _) (Abstraction _ _)) -> printn y
   y @(Application (Term _) (Term _)) -> printn y
   y @(Application (Term _) (Abstraction _ _)) -> printn y
   y @(Application (Application _ _) (Term _)) -> printn y

   y @(Application (Application _ _) (Application _ _)) -> printb y

   y @(Application (Application _ _) (Abstraction _ _)) -> printl y
   y @(Application (Abstraction _ _) (Term _)) -> printl y

   y @(Application (Abstraction _ _) (Application _ _)) -> printr y
   y @(Application (Term _) (Application _ _)) -> printr y

   Abstraction y z -> "\\" ++ y ++ "." ++ (prettify z)
