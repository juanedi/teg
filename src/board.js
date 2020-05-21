class TegBoard extends HTMLElement {
  constructor() {
    super()
    this.shadow = this.attachShadow({mode: 'open'})
  }

  connectedCallback() {
    let embed = document.createElement("object")
    embed.data = this.svgPath
    embed.type = "image/svg+xml"

    let stylesheet = this.children[0]

    embed.addEventListener("load", () => {
      let svgDoc = embed.contentDocument

      if (stylesheet) {
        let defs = svgDoc.getElementsByTagNameNS("http://www.w3.org/2000/svg", "defs")[0]
        if (defs) { defs.appendChild(stylesheet) }
      }

      let countries = Array.
                        from(svgDoc.getElementById("paises").children).
                        flatMap((c) => Array.from(c.children))

      countries.forEach((c) => {
        c.addEventListener("mouseover", (e) => e.target.classList.add("active-country"))
        c.addEventListener("mouseout", (e) => e.target.classList.remove("active-country"))
      })
    })

    this.shadow.appendChild(embed)
  }
}

window.customElements.define("teg-board", TegBoard)
