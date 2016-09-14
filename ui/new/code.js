function newCodeContainer(dbgState, name, id, parent) {

  var codeCont = $('<div/>').addClass('card-content galua-code-table')
  var code     = $('<table/>').addClass('browser-default function')
  codeCont.append(code)

  var closeBtn = $('<a/>')
                 .addClass('btn waves-light waves-effect')
                 .text('close')

  var parentBtn = $('<a/>')
                  .addClass('btn waves-light waves-effect')
                  .text('View Parent')

  var new_tab_id = id ? id : ('fun_tab_' + dbgState.fun_counter++)

  var newLink = $('<li/>')
                .addClass('tab')
                .append($('<a/>').attr('href','#' + new_tab_id).text(name))

  var newPane = $('<div/>')
                .addClass('card')
                .attr('id', new_tab_id)
                .append([ codeCont
                        , $('<div/>').addClass('card-action')
                          .append([parentBtn,$('<span/>').text(' '),closeBtn])
                        ])

  if (!parent) {
      parentBtn.remove()
  } else {
      parentBtn.click(function() {
        var path = '/function'
        jQuery.post(path, { fid: parent }, function(cd) {
          drawFunctionInNewTab(dbgState,cd)
        })
        .fail(disconnected)
        return false
      })
  }

  var tabs = $('#code_tabs')

  closeBtn.click(function() {
    newLink.remove()
    newPane.remove()
    tabs.tabs()
    var x = $('#code_panes :first-child')
    if (x.length !== 0) {
      tabs.tabs('select_tab', x.attr('id'))
    }
    return false
  })


  tabs.append(newLink)
  $('#code_panes').append(newPane)
  tabs.tabs()

  tabs.tabs('select_tab', new_tab_id)

  return code
}



function drawFunctionInNewTab(dbgState, f) {
  var code =
     newCodeContainer(dbgState, f.name, null, f.parent === "" ? null : f.parent)
  drawFunction(dbgState, code, f)
}



function focusCurLine() {
  jQuery.each($('.cur-line:not(.hide)'), function(ix,el) {
    el.scrollIntoView()
  })
}

function drawFunction(dbgState, here, f) {
  here.empty()
  jQuery.each(f.lines, drawLine(dbgState,f.context,f.chunk,here))
  $('.dropdown-button').dropdown()
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


  function addBrkPoint(here, startOn) {
    var brkIcon = $('<i/>').addClass('blue-text material-icons')
                           .css('margin-right','0.5em')
                           .text('pause_circle_filled')
    if (startOn !== true) brkIcon.addClass('hide')

    here.append(brkIcon)
    return {}
  }



  function lineSkeleton() {
    var row  = $('<tr/>')
    var num1 = $('<td/>')
    var num2 = $('<td/>')
    var text = $('<td/>').addClass('code_line')
    row.append(num1,num2,text)
    return { row: row, num1: num1, num2: num2, text: text }
  }

  function lineMenu(lab, clickBrk, clickGoto) {

    function icon(i,help,handler) {
      var link = $('<a/>')
                     .addClass('btn-floating tooltipped')
                     .attr('data-tooltip', help)
                     .click(handler)
      link.tooltip()
      link.append($('<i/>').addClass('material-icons').text(i))

      return $('<li/>').append(link)
    }

    var newId = 'ln_' + dbgState.fun_counter++

    var menu = $('<ul/>')
               .attr('id',newId)
               .addClass('dropdown-content')
               .append( [ icon( 'pause_circle_filled'
                                        , 'Add/remove break-point'
                                        , clickBrk)

                                  , icon ('play_circle_filled'
                                         , 'Continue from here'
                                         , clickGoto)
                                  ] )

    var btn = $('<a/>').text(lab)

    var btn = $('<a/>')
           .addClass('dropdown-button btn')
           .attr('data-activates',newId)
           .text('X')
    return [btn,menu]

            // lab.addClass('btn fixed-action-btn horizontal')
         //     .append(btn,menu)

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
                    .addClass('galua-line-number')
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

        if (context.eid !== null) {
          jQuery.each(t.names, function(ix,cl) {
            it.addClass('exp'+cl)
          })

         if (t.funid !== null) {
              it.addClass('clickable_function');
              it.click(function () {
                jQuery.post('/function', { fid: t.funid },
                    function(code) {
                      drawFunctionInNewTab(dbgState, code)
                    }).fail(disconnected)
                   return false
              })
         }

         if (t.name !== null) {
            it.addClass(t.name.active ? 'active' : 'inactive')
            it.hover(function() {
              $('.exp' + t.name.ref).addClass('gal_highlight_name')
            }, function() {
              $('.exp' + t.name.ref).removeClass('gal_highlight_name')
            })

            if (t.name.active) {
              it.click(function () {
                 jQuery.post('/watchName', { eid: context.eid
                                           , pc: context.pc
                                           , id: t.name.ref
                                           }, renderResult)
                       .fail(disconnected)
                  UIkit.modal($('#value-modal')).show()
               return false
              })
            }
          }

          function renderResult(x) {
            var h = $('#value-header').empty()
            var c = $('#value-container').empty()
            if (x.error) {
              h.text("ERROR")
              c.append(x.error)
            } else {
              h.append($("<span/>").addClass("code_line identifier")
                                   .text(x.name))
              c.append(drawValueEx(dbgState,x.value,true))
            }
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

      // XXX
      var openCur =
            $('input[type="radio"][name=step_type]:checked').val() === 'OpCode'
      if (!(isCurrent && openCur)) skel.row.addClass('uk-hidden')

      var key     = opCodeKey(op)
      subs[i]     = addBrkPoint(num,dbgState.breakPoints[key] === true)
      subs[i].id  = key
      subs[i].row = skel.row
      skel.row.addClass(key)

      if (pcKey === key) skel.row.addClass('cur-line')

      var lineLab = $('<span/>').text(' ' + op.opcode)
      var menu = lineMenu( lineLab
                         , function() { toggleBreakOnOpCode(i, function() {}) }
                         , function() { gotoOpCode(i) } )

      num.append(menu)

      skel.text.text(op.text)

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
          $('.brk_menu.' + id).remove() // -- XXX
        }

        k()
      }

      jQuery.post( hasBrk ? '/removeBreakPoint' : '/addBreakPoint'
                 , { loc: id }, ok)
            .fail(disconnected)

    }
  }
}



