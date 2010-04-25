{-# OPTIONS -Wall -O2 #-}

import VtyWrap(withVty)
import qualified Graphics.Vty as Vty
import Graphics.Vty(Vty)
import Vector2(Vector2(..), (***))
import TermImage(render, string, translate)
import Control.Monad(forM_)
import Control.Applicative(liftA2)
import Data.Monoid(mappend)

main :: IO ()
main = withVty $ \vty -> do
  let diagonal = liftA2 (zipWith Vector2) (concat . cycle) (concat . drop 1 . cycle) [[1..30], [29,28..2]]
      unitCircleStep = map (liftA2 Vector2 cos sin . (/(30::Float)) . fromIntegral) [1 :: Int ..]
      circle = map (fmap truncate . ((*60) *** (*25)) . fmap (+1)) unitCircleStep
  forM_ (zip diagonal circle) $ \(diag, circ) -> do
    let img v = translate v . string Vty.def_attr . concat $ ["Hello world\nHowdy ho! at (", show v, ")"]
    vty `Vty.update` (Vty.pic_for_image . render $
                      img diag
                      `mappend`
                      img circ)
    Vty.next_event vty
