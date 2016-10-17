function icon(cls,i) {
  return $('<i/>').addClass('material-icons ' + cls).text(i)
}

function roundButton(cls,i,help,act) {
  var me = $('<a/>').addClass(cls)
           .addClass('btn-floating waves-effect waves-light tooltipped')
           .css('margin-left','10px')
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
                  .css('height','50px')
                  .css('position','absolute')
                  .css('z-index','10')
                  .append(btns)
                  .hide()
  thing.css('cursor','pointer')
       .addClass('galua-menu-thing')
  $('body').append(container)
  var open = false

  var allBtn = container.find('.btn-floating')
                        .velocity({ scaleX: 0.2, scaleY: 0.2, opacity: 0 }, 0)

  var busy = false

  var timer = null

  function startTimer() {
    stopTimer()
    timer = window.setTimeout(hideMenu, 2 * 1000)
  }

  function stopTimer() {
    if (timer !== null) window.clearTimeout(timer)
    timer = null
  }


  jQuery.each(btns,function(ix,b) {
    b.click(hideMenu).hover(stopTimer,startTimer)
  })

  function showMenu() {
      if (busy) return
      busy = true
      jQuery.each($('.galua-menu-thing'),function(ix,th) {
        if (th !== thing) $(th).data('galua-popup-menu-hide')()
      })
      var thingPagePos = thing.offset()
      var hiddenAbove  = $('body').scrollTop()
      var left = thingPagePos.left + 10
      var top  = thingPagePos.top + thing.height() + 10

      container.css('left', left)
               .css('top', top)
               .show()

      allBtn.velocity({ scaleX: 1, scaleY: 1, opacity: 1 }, 80, 'swing'
                      , function () { open = true; busy = false; startTimer() }
                      )
  }

  function hideMenu() {
    if (busy) return
    busy = true
    stopTimer()
    allBtn.velocity( { scaleX: 0.2, scaleY: 0.2, opacity: 0 }, 80, 'linear'
                   , function() { open = false; busy = false } )
  }

  thing.click(function() { if (open) hideMenu(); else showMenu() })

  return thing.data('galua-popup-menu-hide', hideMenu)
}

