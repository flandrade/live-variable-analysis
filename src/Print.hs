{-# LANGUAGE ScopedTypeVariables #-}

module Print where

import qualified CFG as G
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Printf

-- | Pretty printing of the live variables
showLV :: M.Map Int G.LV -> String
showLV lv = L.foldr prettify "" (reverse $ M.toList lv)
  where
    prettify :: (Int, G.LV) -> String -> String
    prettify eq acc = acc ++ printf "LVIn%s=%s LVOut%s=%s\n" (show (fst eq)) (showSet $ G.lvIn (snd eq)) (show (fst eq)) (showSet $ G.lvOut (snd eq))
      where
        showSet :: S.Set String -> String
        showSet set = show (S.toList set)
