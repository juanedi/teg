class TegBoard extends HTMLElement {
  connectedCallback() {
    let embed = document.createElement("object")
    embed.data = this.svgPath
    embed.type = "image/svg+xml"

    embed.addEventListener("load", () => {
      let svgDoc = embed.contentDocument

      let linkElm = svgDoc.createElementNS("http://www.w3.org/1999/xhtml", "link");
      linkElm.setAttribute("href", "/src/board.css");
      linkElm.setAttribute("type", "text/css");
      linkElm.setAttribute("rel", "stylesheet");
      svgDoc.getElementsByTagNameNS("http://www.w3.org/2000/svg", "defs")[0].appendChild(linkElm);

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
