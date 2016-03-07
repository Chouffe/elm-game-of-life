import Board exposing (update, view, pulsar)
import StartApp.Simple exposing (start)


main =
  start
    { model = pulsar
    , update = update
    , view = view
    }
