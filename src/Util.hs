module Util (guaranteeM) where

import Control.Monad.Trans.Except (ExceptT, throwE)

guaranteeM :: Monad m => ExceptT e m Bool -> e -> ExceptT e m ()
guaranteeM mb e = do
    b <- mb
    if b then return ()
         else throwE e
