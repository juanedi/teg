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
                        from(svgDoc.getElementById("continents").children).
                        flatMap((c) => Array.from(c.children))

      countries.forEach((c) => {
        c.addEventListener("click", (e) => {
          this.dispatchEvent(new CustomEvent("country-clicked", { detail: c.id }))
        })

        c.addEventListener("mouseenter", (e) => {
          e.target.classList.add("active-country")
          this.dispatchEvent(new CustomEvent("country-mouseenter", { detail: c.id }))
        })
        c.addEventListener("mouseleave", (e) => {
          e.target.classList.remove("active-country")
          this.dispatchEvent(new CustomEvent("country-mouseleave", { detail: c.id }))
        })
      })
    })

    this.shadow.appendChild(embed)
  }
}

window.customElements.define("teg-board", TegBoard)
