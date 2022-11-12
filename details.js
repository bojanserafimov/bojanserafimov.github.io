// Fetch all the details element.
const details = document.querySelectorAll(".exclusive>details");

// Add the onclick listeners.
details.forEach((targetDetail) => {
  targetDetail.addEventListener("click", () => {
    // Close all except for ancestors
    details.forEach((detail) => {
      if (!detail.contains(targetDetail)) {
        detail.removeAttribute("open");
      }
    });
  });
});
