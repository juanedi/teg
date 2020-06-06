document.addEventListener('DOMContentLoaded', (event) => {
  const app = Elm.Main.init({
    node: document.getElementById('elm-host'),
    flags: { boardSvgPath: "/map.svg" }
  })

  function initGameSocket(player) {
    console.log("Initializing websocket for player", player)

    window.updatesSocket = new WebSocket(`ws://localhost:5000/game_updates/${player}`);

    updatesSocket.onopen = function (event) {
      console.log("Connection to websocket succeeded!")
    };

    updatesSocket.onmessage = function (event) {
      let newState = JSON.parse(event.data)
      console.log("Got a message!", newState)
      app.ports.portInfo.send({ tag: "game_state_update", data: newState })
    }
  }

  app.ports.sendPortCommand.subscribe(function(cmd) {
    switch(cmd.tag) {
    case "init_game_socket":
      initGameSocket(cmd.data)
      break
    default:
      console.error("Unrecognized command sent through port", cmd)
    }
  })
})
