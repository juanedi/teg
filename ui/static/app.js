document.addEventListener('DOMContentLoaded', (event) => {
  var app = Elm.Main.init({
    node: document.getElementById('elm-host'),
    flags: { boardSvgPath: "/map.svg" }
  });

  window.updatesSocket = new WebSocket("ws://localhost:5000/socket/red");

  updatesSocket.onopen = function (event) {
    console.log("connection open! waiting for local state.")
  };

  updatesSocket.onmessage = function (event) {
    console.log("got a message!", event.data);
  }
})
