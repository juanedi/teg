server: find `which server-exe` | entr -r server-exe --server
watch_haskell: cd server && stack build --file-watch
watch_elm: cd frontend && elm-live --no-server -- src/Main.elm --output=_build/game.js --debug
