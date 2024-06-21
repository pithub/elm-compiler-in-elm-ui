customElements.define('elm-code',
  class extends HTMLElement {
    connectedCallback() {
      try {
        eval(this.getAttribute('code'));
      } catch (error) {
        console.log(error);
      }
    }
  }
);
