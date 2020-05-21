class TegBoard extends HTMLElement {
  constructor() {
    super()
    this.shadow = this.attachShadow({mode: 'open'})
  }

  copyStylesheet(stylesheet, svgDoc) {
    let defs = svgDoc.getElementsByTagNameNS("http://www.w3.org/2000/svg", "defs")[0]
    if (!defs) { return }

    let oldStylesheet = svgDoc.getElementById('cloned-stylesheet')
    if (oldStylesheet) {
      oldStylesheet.innerText = stylesheet.innerText
    } else {
      let clonedStylesheet = stylesheet.cloneNode(true)
      clonedStylesheet.id = "cloned-stylesheet"
      defs.appendChild(clonedStylesheet)
    }
  }

  connectedCallback() {
    let embed = document.createElement("object")
    embed.data = this.svgPath
    embed.type = "image/svg+xml"

    embed.addEventListener("load", () => {
      let svgDoc = embed.contentDocument

      let stylesheet = this.children[0]
      if (stylesheet) {
        // copy the stylesheet generated by elm
        this.copyStylesheet(stylesheet, svgDoc)

        // watch for changes! every time that style channges we need to update the one in the SVG
        const observer = new MutationObserver(() => this.copyStylesheet(stylesheet, svgDoc))
        const config = { characterData: true, attributes: true, childList: true, subtree: true }
        observer.observe(stylesheet, config)
      }

      // attach event handlers
      let countries = Array.
                        from(svgDoc.getElementById("continents").children).
                        flatMap((c) => Array.from(c.children))

      countries.forEach((c) => {
        c.addEventListener("click", (e) => {
          this.dispatchEvent(new CustomEvent("country-clicked", { detail: c.id }))
        })

        c.addEventListener("mouseenter", (e) => {
          this.dispatchEvent(new CustomEvent("country-mouseenter", { detail: c.id }))
        })
        c.addEventListener("mouseleave", (e) => {
          this.dispatchEvent(new CustomEvent("country-mouseleave", { detail: c.id }))
        })
      })
    })

    this.shadow.appendChild(embed)
  }
}

window.customElements.define("teg-board", TegBoard)
