// [Appearance]
// Selecting Tabs:Integration > Process
$(document).on('click', '.process-entry', () => {
  $('.process-entry').removeClass('active');
  $(this).addClass('active');
});
