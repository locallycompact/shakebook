$(function () {

  // NAV

  var $sidebar = $('nav');
  var $toggle = $('#toggle');

  $toggle.click(function () {
    $sidebar.toggleClass('expanded');
  });

  $sidebar.find('a').click(function () {
    $sidebar.removeClass('expanded');
  });

  // TABLES
  $('#main table').each(function () {
    var $table = $(this).wrap('<div class="overflow-table"></div>');
  });

});
