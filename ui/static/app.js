document.addEventListener('DOMContentLoaded', (event) => {
  var app = Elm.Main.init({
    node: document.getElementById('elm-host'),
    flags: { boardSvgPath: "/map.svg" }
  });

  app.ports.initSocket.subscribe(function(player) {
    console.log("Initializing websocket for player", player)

    window.updatesSocket = new WebSocket(`ws://localhost:5000/game_updates/${player}`);

    updatesSocket.onopen = function (event) {
      console.log("Connection to websocket succeeded!")
    };

    updatesSocket.onmessage = function (event) {
      let newState = JSON.parse(event.data)
      console.log("Got a message!", newState)
      app.ports.stateUpdates.send(newState)
    }
  });
})
