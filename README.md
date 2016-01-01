# purescript-apparch

AppArch is a lightweight library heavily inspired by [the Elm Architecture][elm-arch] and Elm's [start-app][start-app]. The library relies on [React components][react] and is powered [purescript-signal][purescript-signal].

## Usage Example

```purescript
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
```

[react]: https://facebook.github.io
[start-app]: https://github.com/evancz/start-app
[elm-arch]: https://github.com/evancz/elm-architecture-tutorial/
[purescript-signal]: https://github.com/bodil/purescript-signal
