document.addEventListener("DOMContentLoaded", function () {
  import("https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs")
    .then((module) => {
      const mermaid = module.default || module;
      mermaid.initialize({ startOnLoad: true });
    })
    .catch((err) => console.error("Failed to load mermaid:", err));
});
