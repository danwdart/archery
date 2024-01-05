{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-safe -Wno-unsafe -Wno-missing-safe-haskell-mode #-}

module Data.Aeson.Compat where
#if !MIN_VERSION_aeson(2,1,2)
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Maybe

throwDecode ∷ FromJSON a ⇒ BSL.ByteString → a
throwDecode = fromJust . decode
#endif
