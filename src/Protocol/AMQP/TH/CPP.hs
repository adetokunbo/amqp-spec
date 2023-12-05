{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Protocol.AMQP.TH.CPP (
  compatConP,
  compatDoE,
) where

import Language.Haskell.TH (Pat (..))
import Language.Haskell.TH.Syntax (Exp (..), Name, Stmt)


#if MIN_VERSION_template_haskell(2,18,0)
-- Allow a list of type applications preceding argument patterns
compatConP :: Name -> [Pat] -> Pat
compatConP x = ConP x []
#else
compatConP :: Name -> [Pat] -> Pat
compatConP  = ConP
#endif

#if MIN_VERSION_template_haskell(2,17,0)
-- Support for Qualified Do
compatDoE :: [Stmt] -> Exp
compatDoE x = DoE Nothing x
#else
compatDoE :: [Stmt] -> Exp
compatDoE = DoE
#endif
