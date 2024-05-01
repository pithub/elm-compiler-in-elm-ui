customElements.define('eval-elm',
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
