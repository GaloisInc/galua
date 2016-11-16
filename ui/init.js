(function($) {
  $(document).ready(function() {
    $('.modal-trigger').leanModal()
    $('.tooltipped').tooltip({delay:1000})

    jQuery.get('/view', drawDebugger)
          .fail(disconnected)

    $('#text-expression-text').keypress(textExpressionBoxKeyPressed)
  })
})(jQuery)


