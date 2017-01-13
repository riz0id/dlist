{-# LANGUAGE OverloadedStrings #-}

module OverloadedStrings (test) where

import qualified Data.DList as DList
#if MIN_VERSION_base(4,9,0)
import qualified Data.DList.DNonEmpty as DNonEmpty
#endif

test :: IO ()
test = do
  -- 'show' is essential for these tests.
  putStrLn $ show $ "OverloadedStrings(DList):" `DList.append` " success"
#if MIN_VERSION_base(4,9,0)
  putStrLn $ show $ "OverloadedStrings(DNonEmpty):" `DNonEmpty.append` " success"
#endif
