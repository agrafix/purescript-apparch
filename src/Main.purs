module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console

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

type EffModel eff m a =
  { model :: m
  , effects :: Effects eff a
  }

type Effects eff a = Array (Eff (dom :: DOM, chan :: Chan | eff) a)

noFx :: forall eff m a. m -> EffModel eff m a
noFx m = { model: m, effects: noEffects }

noEffects :: forall eff a. Effects eff a
noEffects = []

type Config eff m a =
  { init :: EffModel eff m a
  , update :: a -> m -> EffModel eff m a
  , view :: ReactClass (CompProps m a)
  , inputs :: Array (S.Signal a)
  , renderTarget :: ElementId
  }

runApp :: forall eff m a. Config eff m a -> Eff (dom :: DOM, chan :: Chan | eff) Unit
runApp config = do
  actionChan <- channel []
  let actionSignal = subscribe actionChan
      inputs =
        fromJust $
        S.mergeMany (actionSignal : map (map singleton) config.inputs)
      updateStep action effModel =
        let newEffModel = config.update action effModel.model
        in { model: newEffModel.model
           , effects: effModel.effects ++ newEffModel.effects
           }
      update actions effModel =
        foldl (flip updateStep) (noFx effModel.model) actions
      effModelSignal =
        S.foldp update config.init inputs
  tgt <- findRenderTarget config.renderTarget
  let renderRunModel effModel = do
         let wiredEffects =
                 flip map effModel.effects $ \runEff -> do
                 res <- runEff
                 send actionChan [res]
             m = effModel.model
             props =
               { model: m
               , address: actionChan
               }
             factory =
               createFactory config.view props
         sequence_ wiredEffects
         _ <- render factory tgt
         return unit
      renderSignal =
        map renderRunModel effModelSignal
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
  | Nop

initModel :: Model
initModel = { counter: 0 }

update :: forall eff. Action -> Model -> EffModel (console :: CONSOLE | eff) Model Action
update act m =
  case act of
    Nop -> noFx m
    Increment ->
      { model: m { counter = m.counter + 1 }
      , effects:
          [ do log "Increment"
               return Nop
          ]
      }
    Decrement -> noFx $ m { counter = m.counter - 1 }

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
   { init: noFx initModel
   , update: update
   , view: view
   , inputs: []
   , renderTarget: ElementId "app"
   }
