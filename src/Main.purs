module Main where

import Prelude
import Control.Monad.Eff

import Data.Maybe.Unsafe (fromJust)
import Data.Nullable
import Data.Foldable
import Data.Array

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(..), Element())

import qualified Signal as S
import Signal.Channel

import React

import qualified React.DOM as D
import qualified React.DOM.Props as P

type CompProps m a =
  { model :: m
  , address :: Channel (Array a)
  }

type Config m a =
  { init :: m
  , update :: a -> m -> m
  , view :: ReactClass (CompProps m a)
  , inputs :: Array (S.Signal a)
  , renderTarget :: ElementId
  }

runApp :: forall m a e. Config m a -> Eff (dom :: DOM, chan :: Chan | e) Unit
runApp config = do
  actionChan <- channel []
  let actionSignal = subscribe actionChan
      inputs =
        fromJust $
        S.mergeMany (actionSignal : map (map singleton) config.inputs)
      modelSignal =
        S.foldp (\actions m -> foldl (flip config.update) m actions) config.init inputs
  tgt <- findRenderTarget config.renderTarget
  let renderModel m = do
         let props =
               { model: m
               , address: actionChan
               }
             factory =
               createFactory config.view props
         _ <- render factory tgt
         return unit
      renderSignal =
        map renderModel modelSignal
  S.runSignal renderSignal

findRenderTarget :: forall e. ElementId -> Eff (dom :: DOM | e) Element
findRenderTarget elemId = do
  win <- window
  doc <- document win
  elem <- getElementById elemId (htmlDocumentToNonElementParentNode doc)
  return (fromJust $ toMaybe elem)

{-
EXAMPLE
-}

type Model = { counter :: Int }

data Action
  = Increment
  | Decrement

initModel :: Model
initModel = { counter: 0 }

update :: Action -> Model -> Model
update act m =
  case act of
    Increment -> m { counter = m.counter + 1 }
    Decrement -> m { counter = m.counter - 1 }

view :: ReactClass (CompProps Model Action)
view = createClass $ spec unit \ctx -> do
  p <- getProps ctx
  return $
    D.p
      [ P.className "Counter"
      ]
      [ D.text (show p.model.counter)
      , D.button
          [ P.onClick (\_ -> send p.address [Increment]) ]
          [ D.text " Click me to increment!" ]
      , D.button
          [ P.onClick (\_ -> send p.address [Decrement]) ]
          [ D.text " Click me to decrement!" ]
      ]

main =
  runApp
   { init: initModel
   , update: update
   , view: view
   , inputs: []
   , renderTarget: ElementId "app"
   }
