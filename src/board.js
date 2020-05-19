class TegBoard extends HTMLElement {
  connectedCallback() {
    var embed = document.createElement("object")
    embed.data = this.svgPath
    embed.type = "image/svg+xml"
    this.appendChild(embed)
  }
}

window.customElements.define("teg-board", TegBoard)
