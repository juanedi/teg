document.addEventListener('DOMContentLoaded', (event) => {
  const app = Elm.Main.init({
    node: document.getElementById('elm-host'),
    flags: { boardSvgPath: "/map.svg" }
  })

  const sockets = {}

  function initLobbySocket() {
    console.log("lobby socket: initializing")

    sockets.lobby = new WebSocket("ws://localhost:5000/lobby")

    sockets.lobby.onopen = function (event) {
      console.log("lobby socket: connection succeeded!")
    }

    sockets.lobby.onmessage = function (event) {
      let msg = JSON.parse(event.data)
      console.log("lobby socket: got a message", msg)
      app.ports.portInfo.send({ tag: "lobby_state_update", data: msg })
    }
  }

  function initGameSocket(player) {
    console.log("Initializing websocket for player", player)

    sockets.gameUpdates = new WebSocket(`ws://localhost:5000/game_updates/${player}`)

    sockets.gameUpdates.onopen = function (event) {
      console.log("Connection to websocket succeeded!")
    }

    sockets.gameUpdates.onmessage = function (event) {
      let newState = JSON.parse(event.data)
      console.log("Got a message!", newState)
      app.ports.portInfo.send({ tag: "game_state_update", data: newState })
    }
  }

  app.ports.sendPortCommand.subscribe(function(cmd) {
    switch(cmd.tag) {
    case "init_lobby_socket":
      initLobbySocket()
      break
    case "init_game_socket":
      sockets.lobby.close()
      initGameSocket(cmd.data)
      break
    default:
      console.error("Unrecognized command sent through port", cmd)
    }
  })
})
