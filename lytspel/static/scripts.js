// Collapse the navbar if the user clicks elsewhere.
function collapseNavbar() {
  $('.collapse').collapse('hide');
}

document.body.addEventListener('click', collapseNavbar);
