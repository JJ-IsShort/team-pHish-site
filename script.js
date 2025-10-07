var menu_pressed = false;
var sidebar = null;
var sidebar_button = null;

function press_menu() {
  if (sidebar == null) {
    sidebar = document.getElementById("sidebar");
  }
  if (sidebar_button == null) {
    sidebar_button = document.getElementById("sidebar-button");
  }
  menu_pressed = !menu_pressed;
  sidebar.classList.toggle("translate-x-full");
}
