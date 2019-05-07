function auto_grow(element) {
    element.style.height = Math.max(element.scrollHeight) + "px";
}

function load_nav() {
  fetch('/nav').then(
    response => response.text().then(
      resp => document.getElementById("nav").innerHTML = resp));
}

load_nav();
