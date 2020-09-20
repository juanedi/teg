document.addEventListener('DOMContentLoaded', (event) => {
  const app = Elm.Main.init({
    node: document.getElementById('elm-host'),
    flags: window.TEG_FLAGS
  })

  let socket = null

  function initSocket(url) {
    console.log("initializing game socket")

    socket = new WebSocket(url)

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

  function copyText(containerId) {
    let element = document.getElementById(containerId)
    if (element) {
      element.select()
      document.execCommand("copy")
    }
  }

  app.ports.sendPortCommand.subscribe(function(cmd) {
    switch(cmd.tag) {
    case "init_socket":
      initSocket(cmd.url)
      break
    case "send":
      sendMsg(cmd.msg)
      break
    case "copy":
      copyText(cmd.containerId)
      break
    default:
      console.error("Unrecognized command sent through port", cmd)
    }
  })
})
