function drawFunctionInNewTab(dbgState, f) {

  var code = $('<table/>').addClass('code_box')
  drawFunction(dbgState, code, f)

  var parentBtn = $('<span/>')
                 .addClass('uk-icon-reply')
                 .css('margin-left', '0.5em')

  var closeBtn = $('<span/>')
                 .addClass('uk-close')
                 .css('margin-left', '0.5em')

  var newLink = $('<li/>')
                .append($('<a/>')
                        .append([ $('<span/>').text(f.name), parentBtn, closeBtn ])
                       )

  var newPane = $('<li/>').append(code)

  if (f.parent === "") {
      parentBtn.remove()
  } else {
      parentBtn.click(function() {
        var path = '/function'
        jQuery.post(path, { fid: f.parent }, function(code) {
          drawFunctionInNewTab(dbgState,code)
        })
        .fail(disconnected)
        return false
      })
  }

  closeBtn.click(function() {
    newLink.remove()
    newPane.remove()
    return false
  })

  $('#code_pane_control').append(newLink)
  $('#code_pane').append(newPane)

}

function focusCurLine() {
  jQuery.each($('.cur-line:not(.uk-hidden)'), function(ix,el) {
    el.scrollIntoView()
  })
}

function drawFunction(dbgState, here, f) {
  here.empty()
  jQuery.each(f.lines, drawLine(dbgState,f.context,f.chunk,here))
}


function drawLine(dbgState,context,chunkId,here) {

  // This is how we identify a location to the server
  function mkOpKey(fid,c) {
     if (fid !== undefined && c !== undefined) return fid + '-' + c
     return null
  }
  function opCodeKey(o) { return mkOpKey(o.path,o.opcode) }


  // Utility: set or remove a class, depending on the boolean.
  function setClass(x,y,b) { if (b) x.addClass(y); else x.removeClass(y) }


  // Identifies which op-codes belong to a source line, so that
  // we may expand and collapse them.
  function mkClass(x) { return 'ops_' + x }



  function addSpinner(here) {
    var spinner = $('<i/>').addClass('uk-icon-gear uk-icon-spin uk-hidden')
    here.prepend(spinner)
    return spinner
  }

  function addBrkPoint(here, startOn) {
    var brkIcon = $('<i/>')
                  .addClass('uk-icon-arrow-circle-right uk-text-primary')
                  .css('margin-right','0.5em')
                  .addClass('breakpoint')
    if (startOn !== true) brkIcon.addClass('uk-hidden')

    here.append(brkIcon)
    return {}
  }



  function lineSkeleton() {
    var row   = $('<tr/>')
    var num1  = $('<td/>')
                .addClass('uk-block-muted uk-text-muted')
                .addClass('uk-text-middle uk-text-right')
                .css('padding-left', '5px')

    var num2 = $('<td/>').addClass('uk-block-muted uk-text-muted')
                         .addClass('uk-text-middle uk-text-right')
                         .css('padding-left', '5px')

    var text = $('<td/>').addClass('uk-text-left uk-text-nowrap code_line')

    row.append(num1,num2,text)

    return { row: row, num1: num1, num2: num2, text: text }
  }

  function lineMenu(lab, clickBrk, clickGoto) {

    function icon(i,help,handler) {
      return $('<i/>')
             .addClass('uk-icon-' + i)
             .addClass('uk-icon-hover uk-icon-small')
             .attr('title',help)
             .attr('data-uk-tooltip','{pos:"top"}')
             .css('margin-right','0.5em')
             .click(handler)
    }

    var icons = [ icon( 'arrow-circle-right'
                      , 'Add/remove break-point'
                      , clickBrk)

                , icon ('rocket', 'Continue from here', clickGoto)
                ]

    var menu = $('<div/>')
               .addClass('uk-dropdown uk-dropdown-small min-width-auto')
               .append(icons)

    var small = $('<div/>')
                 .attr('data-uk-dropdown','{pos:"right-center", remaintime:100}')
                 .addClass('uk-button-dropdown')
                 .append([lab.addClass('uk-text-bold'),menu])

    return small
  }




  return function(unused_ix,line) {

    var srcLineClass = ''
    if (line.line !== null) srcLineClass = 'src_' + chunkId + '-' + line.line

    var subs    = []
    var theLine = null

    // Compute if the source line has break points.
    // A source line has a break point, if any of its opcodes do.
    var breakNum = 0
    var isCurrent = false
    var pcKey = dbgState.programCounter === null? ''
              : mkOpKey(dbgState.programCounter.fid,
                        dbgState.programCounter.pc)

    jQuery.each(line.opcodes, function(i,o) {
      var key = opCodeKey(o)
      if (dbgState.breakPoints[key] === true) ++breakNum
      if (key === pcKey) isCurrent = true
    })

    if (line.line !== null) drawSrcLine()
    jQuery.each(line.opcodes,drawOpCode)

    // Draw the main srouce line
    function drawSrcLine() {

      var skel = lineSkeleton()
      var num  = skel.num1
      var text = skel.text

      if (isCurrent) skel.row.addClass('cur-line')
      skel.row.addClass(srcLineClass)

      theLine = addBrkPoint(num, breakNum !== 0)

      var lineLab = $('<span/>')
                    .addClass('uk-text-small')
                    .text(' ' + line.line)


      if (line.opcodes.length > 0) {
        var menu = lineMenu(lineLab,toggleBreakOnSourceLine,gotoSourceLine)
        num.append(menu)
        text
        .css('cursor','pointer')
        .attr( 'data-uk-toggle'
             , JSON.stringify({ target: '.' + mkClass(line.line)
                              , animation: 'uk-animation-slide-top'
                             })
             )
      } else num.append(lineLab)

      jQuery.each(line.text, function(ix,t) {
        var it = $('<span/>')
                 .text(t.lexeme)
                 .addClass(t.token)

        if (context !== null) {
          jQuery.each(t.names, function(ix,cl) {
            it.addClass('exp'+cl)
          })

         if (t.name !== null) {
            it.hover(function() {
              $('.exp' + t.name).addClass('gal_highlight_name')
            }, function() {
              $('.exp' + t.name).removeClass('gal_highlight_name')
            }).click(function () {
                 jQuery.post('/watchName', { eid: context.eid
                                           , pc: context.pc
                                           , id: t.name
                                           }, renderResult)
                       .fail(disconnected)
                  UIkit.modal($('#value-modal')).show()
               return false
            })
          }

          function renderResult(x) {
            $('#value-container')
            .empty().append(drawValue(dbgState,x))
          }

        }

        text.append(it)
      })
      here.append(skel.row)
    }

    function gotoSourceLine() {
        if (line.opcodes.length > 0) gotoOpCode(0)
    }


    // When we click a source line we either:
    //  1. Remove all break points (if any), or
    //  2. Add a break point to the first instruction.
    function toggleBreakOnSourceLine() {

      var todo = []
      jQuery.each(line.opcodes, function(i,o) {
        if (dbgState.breakPoints[opCodeKey(o)] === true) todo.push(i)
      })

      if (todo.length > 0) {

        // Turn off all the op-codes that have a break point.
        function next(i) {
          if (i >= todo.length) { return }
          toggleBreakOnOpCode(todo[i], function() { next(i+1) })
        }
        next(0)

      } else {

        // If no op-code has a break-point, then put a break-point on the first
        if (line.opcodes.length > 0) toggleBreakOnOpCode(0,function(){})
      }
    }


    // Draw a code-viwere line
    function drawOpCode(i,op) {
      var skel = lineSkeleton()
      var num  = skel.num2
      num.empty()
      // num.css('cursor', 'pointer')

      if (line.line !== null) skel.row.addClass(mkClass(line.line))
      var openCur =
            $('input[type="radio"][name=step_type]:checked').val() === 'OpCode'
      if (!(isCurrent && openCur)) skel.row.addClass('uk-hidden')

      var key     = opCodeKey(op)
      subs[i]     = addBrkPoint(num,dbgState.breakPoints[key] === true)
      subs[i].id  = key
      subs[i].row = skel.row
      skel.row.addClass(key)

      if (pcKey === key) skel.row.addClass('cur-line')

      var lineLab = $('<span/>').addClass('uk-text-small').text(' ' + op.opcode)
      var menu = lineMenu( lineLab
                         , function() { toggleBreakOnOpCode(i, function() {}) }
                         , function() { gotoOpCode(i) } )

      num.append(menu)

      skel.text.addClass('uk-text-muted uk-text-small')
               .css('padding-left', '1em')
               .text(op.text)

      here.append(skel.row)
    }

    function gotoOpCode(i) {
      var id      = subs[i].id

      function ok() {
        $('.cur-line').removeClass('cur-line')
        subs[i].row.addClass('cur-line')
        $('.'+srcLineClass).addClass('cur-line')
      }

      jQuery.post( '/goto', { loc: id }, ok)
            .fail(disconnected)

    }

    // Toggles the breakpoint on the given op-code
    function toggleBreakOnOpCode(i,k) {
      var me      = subs[i]
      var id      = me.id
      var hasBrk  = dbgState.breakPoints[id] === true

      // Server interaction went OK
      function ok(res) {
        hasBrk          = !hasBrk
        dbgState.breakPoints[id] = hasBrk
        setClass($('.' + id + ' .breakpoint'), 'uk-hidden', !hasBrk)

        if (theLine !== null) {
            var lineHasBrk = false
            jQuery.each($('.' + mkClass(line.line + ' .breakpoint')),
                function(ix,it) {
                  if (! $(it).hasClass('uk-hidden')) {
                    lineHasBrk = true
                    return false // stop
                  }
                  return true // keep going
                })

            setClass($('.' + srcLineClass + ' .breakpoint'),
                                              'uk-hidden', !lineHasBrk)

        }

        if (hasBrk) {
          console.log(res)
          $('#breakpoints-list').append(drawBreakPoint(dbgState,res))
        } else {
          $('.brk_menu.' + id).remove()
        }

        k()
      }

      jQuery.post( hasBrk ? '/removeBreakPoint' : '/addBreakPoint'
                 , { loc: id }, ok)
            .fail(disconnected)

    }
  }
}



