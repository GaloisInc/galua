function textExpressionBoxKeyPressed(ev) {
  if (ev.keyCode === 13 && !ev.shift) {
    $('#text-expression-text').data('galua-new-text-expression')()
    return false // stop propagation, we handled it
  }
  return true // propagate as normal
}


function editExpression(title,initVal,onDone) {
  var pane  = $('#text-expression-pane')
  var txt   = $('#text-expression-text')
  $('#text-expression-pane-title').empty().text(title)

  txt.val(initVal)
  txt.data('galua-new-text-expression', function() {
    onDone(txt.val())
    pane.closeModal()
  })

  pane.openModal()

}


