module AppArch
  ( CompProps(..)
  , EffModel(..), Effects(..), noFx, noEffects
  , Config(..), runApp
  )
where

import Control.Monad.Eff
import Data.Array
import Data.Foldable
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable
import Prelude

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToNonElementParentNode)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (ElementId(), Element())

import Signal.Channel
import qualified Signal as S

import React

-- |Properties of a view
type CompProps m a =
  { model :: m
  , address :: Channel (Array a)
  }

-- |A model and possible effects
type EffModel eff m a =
  { model :: m
  , effects :: Effects eff a
  }

-- |A collection of effects
type Effects eff a = Array (Eff (dom :: DOM, chan :: Chan | eff) a)

-- |Combine a model with an empty effect collection
noFx :: forall eff m a. m -> EffModel eff m a
noFx m = { model: m, effects: noEffects }

-- |No effects
noEffects :: forall eff a. Effects eff a
noEffects = []

-- |Configuration and entrypoint of the application
type Config eff m a =
  { init :: EffModel eff m a
  , update :: a -> m -> EffModel eff m a
  , view :: ReactClass (CompProps m a)
  , inputs :: Array (S.Signal a)
  , renderTarget :: ElementId
  }

-- |Launch the application
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
