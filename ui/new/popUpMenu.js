function icon(cls,i) {
  return $('<i/>').addClass('material-icons ' + cls).text(i)
}

function roundButton(cls,i,help,act) {
  var me = $('<a/>').addClass(cls)
           .addClass('btn-floating waves-effect waves-light tooltipped')
           .css('margin-right','10px')
           .attr('data-tooltip',help)
           .append(icon('',i))
           .click(act)
  me.tooltip()
  return me
}



function makeMenu(thing,btns) {
  var openWidth = btns.length * 50
  var container = $('<div/>')
                  .css('width', openWidth + 'px')
                  .css('height','40px')
                  .css('position','absolute')
                  .css('left','0px')
                  .css('top','0px')
                  .append(btns)
                  .hide()
  thing.css('cursor','pointer')
  var open = false

  var outer = $('<div/>').css('display','inline-block')
                         .css('position','relative')

  function showMenu() {
      container.css('left',(thing.width() + 10) + 'px')
      outer.addClass('pop_up_open')
      container.show()
      open = true
  }

  function hideMenu() {
    container.hide()
    outer.removeClass('pop_up_open')
    open = false
  }

  thing.click(function() { if (open) hideMenu(); else showMenu() })
  jQuery.each(btns,function(ix,b) { b.click(hideMenu) })

  return outer.append([thing,container])
}

