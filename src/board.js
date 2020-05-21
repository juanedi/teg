class TegBoard extends HTMLElement {
  connectedCallback() {
    let embed = document.createElement("object")
    embed.data = this.svgPath
    embed.type = "image/svg+xml"

    let stylesheet = this.previousSibling

    embed.addEventListener("load", () => {
      let svgDoc = embed.contentDocument

      svgDoc.getElementsByTagNameNS("http://www.w3.org/2000/svg", "defs")[0].appendChild(stylesheet);

      let countries = Array.
                        from(svgDoc.getElementById("paises").children).
                        flatMap((c) => Array.from(c.children))

      countries.forEach((c) => {
        c.addEventListener("mouseover", (e) => e.target.classList.add("active-country"))
        c.addEventListener("mouseout", (e) => e.target.classList.remove("active-country"))
      })
    })

    this.appendChild(embed)
  }
}

window.customElements.define("teg-board", TegBoard)
