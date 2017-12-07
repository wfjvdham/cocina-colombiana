$(document).on('click', '.needed', function () {
  Shiny.onInputChange('last_btn',this.id);
});
