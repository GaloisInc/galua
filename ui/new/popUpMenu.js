function icon(cls,i) {
  return $('<i/>').addClass('material-icons ' + cls).text(i)
}

function roundButton(cls,i,help,act) {
  var me = $('<a/>').addClass(cls)
           .addClass('btn-floating waves-effect waves-light tooltipped')
           .attr('data-tooltip',help)
           .append(icon('',i))
           .click(act)
  me.tooltip()
  return me
}



function makeMenu(thing,btns) {
  return popUpMenu($('<div/>')
                   .append([ thing.addClass('controller')
                           , $('<div/>')
                             .css('height','0')
                             .addClass('menu').append(btns)
                           ]))
}

function popUpMenu(thing) {

  var active  = 0
  var opening = true


  var controller = thing.find('.controller')
  var btns       = thing.find('.menu .btn-floating')

  var timeOut = null


  btns.css('position','absolute')
      .css('left','0')
      .css('top','0')
      .css('width','0')
      .css('height','0')
      .hover(resetTimeout,closeInAWhilte)
      .click(hov(false))

  controller.css('cursor','pointer')
            .hover(hov(true),closeInAWhilte)
            .click(toggle)


  return thing.css('display','inline-block')
              .css('position','relative')

  function resetTimeout() {
    if (timeOut === null) return
    window.clearTimeout(timeOut)
  }

  function closeInAWhilte() {
    timeOut = window.setTimeout(hov(false), 1500)
  }

  function hov(open) {
    return function() { if (opening === open) toggle() }
  }

  function toggle() {

    if (active > 0) return
    active = btns.length
    var w = controller.width()
    if (opening) { btns.css('left', w + 'px') }

    jQuery.each(btns,function(ix,btn) {
      $(btn).velocity(opening?
                      { left: w + 18 + ix * 50 + 'px', width: 40, height: 40 }
                    : { left: w, width: 0, height: 0 }
                    , { easing: 'linear'
                      , duration: 'fast'
                      , queue: false
                      , complete:
                          function() { if (active === 1) opening = !opening
                                       --active
                                     }
                     })
    })
  }

}
