document.addEventListener("DOMContentLoaded", function () {
  // Select all links in the book menu
  const menuLinks = document.querySelectorAll(".book-menu a");

  menuLinks.forEach((link) => {
    // Remove target attribute to prevent new tab
    link.removeAttribute("target");

    // Add click event to ensure current tab navigation
    link.addEventListener("click", function (event) {
      // Prevent default to control navigation
      event.preventDefault();

      // Navigate in current tab
      window.location.href = this.href;
    });
  });
});
