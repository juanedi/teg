document.addEventListener('DOMContentLoaded', (event) => {
  const app = Elm.Main.init({
    node: document.getElementById('elm-host'),
    flags: {
      viewport: { width: window.innerWidth, height: window.innerHeight},
      boardSvgPath: "/map.svg"
    }
  })

  let socket = null

  function initSocket() {
    console.log("initializing game socket")

    socket = new WebSocket(`ws://localhost:5000/ws/`)

    socket.onopen = function (event) {
      console.log("socket: connection succeeded!")
    }

    socket.onmessage = function (event) {
      const update = JSON.parse(event.data)
      app.ports.portInfo.send(update)
    }
  }

  function sendMsg(msg) {
    if (socket) {
      socket.send(JSON.stringify(msg))
    } else {
      console.error("Tried to send a command to the server before the socket was initialized")
    }
  }

  app.ports.sendPortCommand.subscribe(function(cmd) {
    switch(cmd.tag) {
    case "init_socket":
      initSocket()
      break
    case "send":
      sendMsg(cmd.msg)
      break
    default:
      console.error("Unrecognized command sent through port", cmd)
    }
  })
})
