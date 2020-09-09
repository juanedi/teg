document.addEventListener('DOMContentLoaded', (event) => {
  const app = Elm.Main.init({
    node: document.getElementById('elm-host'),
    flags: {
      viewport: { width: window.innerWidth, height: window.innerHeight},
      boardSvgPath: "/map.svg"
    }
  })

  function initSocket() {
    console.log("initializing game socket")

    const socket = new WebSocket(`ws://localhost:5000/ws/`)
    // TODO: don't expose this
    window.socket = socket

    socket.onopen = function (event) {
      console.log("socket: connection succeeded!")
    }

    socket.onmessage = function (event) {
      const update = JSON.parse(event.data)
      console.log("socket: got a message", update)
      app.ports.portInfo.send(update)
    }
  }

  app.ports.sendPortCommand.subscribe(function(cmd) {
    switch(cmd.tag) {
    case "init_socket":
      initSocket()
      break
    case "send":
      // TODO: send message!
      break
    default:
      console.error("Unrecognized command sent through port", cmd)
    }
  })
})
