(function($) {
  $(document).ready(function() {
    $('.modal-trigger').leanModal()
    $('.tooltipped').tooltip({delay:1000})

    jQuery.get('/view', drawDebugger(false))
          .fail(disconnected)
    doPoll(0)

  })
})(jQuery)


