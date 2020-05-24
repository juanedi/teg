server: find `which teg` | entr -r teg --server
watch_haskell: stack build --file-watch
watch_elm: cd ui && elm-live --no-server -- src/Main.elm --output=_build/game.js --debug
