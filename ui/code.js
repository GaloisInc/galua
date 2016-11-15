
function newCodeContainer(dbgState, name, id, parent) {

  var codeCont = $('<div/>').addClass('card-content galua-code-table')
  var code     = $('<div/>').addClass('function')
  codeCont.append(code)

  var closeBtn = id ? null :
               $('<a/>')
               .addClass('btn waves-light waves-effect')
               .text('close')

  var parentBtn = !parent ? null
                : $('<a/>')
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

  if (parentBtn) {
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

  if (closeBtn) {
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
  }


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
  jQuery.each($('.cur-line:visible'), function(ix,el) {
    el.scrollIntoView()
  })
}




function drawFunction(dbgState, here, f) {
  var fid = dbgState.programCounter.fid

  // Check if we already drew this function.  If so, we just "fix up" the
  // already draw version, which speeds things up considerably.
  var drawnFun = here.data('galua-function')
  if (drawnFun && fid === drawnFun) {
    redrawFunction(dbgState,here,f)
    return
  }

  here.empty()
  jQuery.each(f.lines, drawLine(dbgState,f.context,f.chunk,here))
  $('.dropdown-button').dropdown()
  here.data('galua-function', fid)
}


/* Draw a function in "quick" mode.  This assumes that `here` already
contains the function, and we just need to update it with the parts
of `f` that do actually change. */
function redrawFunction(dbgState, here, f) {

  // Fix up the current line
  var pc = dbgState.programCounter.pc
  var done = false
  jQuery.each(f.lines,function(line_ix,line) {
    if (done) return false
    jQuery.each(line.opcodes,function(op_ix,opcode) {
      if (opcode.opcode === pc) {
        // Remember old current line
        var cl = $('.cur-line.galua-source-line')

        // Turn off currently selected
        $('.cur-line').removeClass('cur-line')

        var cid = 'src_' + f.chunk + '-' + line.line

        // Highlight new current source line
        $('.' + cid)
        .addClass('cur-line')

        // Highlight new current op-code
        $('.ops_' + line.line + '.' + f.chunk + '-' + opcode.opcode)
        .addClass('cur-line')

        // Open/close opcode if we moved onto a new line
        if ($('#step-by-opcode').is(':checked') && !cl.hasClass(cid)) {
          var old_line = cl.data('galua-line')
          $('.galua-opcode-line.ops_' + old_line).hide()
          $('.galua-opcode-line.ops_' + line.line).show()
        }

        done = true
        return false
      }
    })

    // Fix up the "in scope"
    jQuery.each(line.text, function(ix,t) {
      if (t.name === null) return true
      $('#' + t.name.ref)
      .removeClass('active inactive')
      .addClass(t.name.active ? 'active' : 'inactive')
      .data('galua-pc',pc)
    })
  })


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
    var brkIcon = $('<i/>').addClass('tiny blue-text material-icons')
                           .text('pause_circle_filled')
                           .css('margin-left', '-1em')
                           .addClass('breakpoint')
    setClass(brkIcon, 'hide', startOn !== true)
    here.append(brkIcon)
  }



  function lineSkeleton(ty) {
    var row  = $('<div/>').addClass(ty + ' row').css('margin','0')
    var num  = $('<div/>').addClass('left-align col s1 galua-line-number')
    var text = $('<div/>').addClass('col s10 code_line lift-align')
    row.append([num,text])
    return { row: row, num: num, text: text }
  }

  function lineMenu(lab, clickBrk, clickEditBrk, clickGoto) {

    return makeMenu ( lab.css('font-weight','bold')
                    , [ roundButton( 'blue white-text'
                                   , 'pause'
                                   , 'Add/remove break-point'
                                   , clickBrk )

                      , roundButton ( 'amber white-text'
                                    , 'edit'
                                    , 'Edit break-point condition'
                                    , clickEditBrk
                                    )

                      , roundButton ( 'green white-text'
                                    , 'play_arrow'
                                    , 'Continue from here'
                                    , clickGoto)

                      ]
                    )

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
      if (dbgState.breakPoints[key] !== undefined) ++breakNum
      if (key === pcKey) isCurrent = true
    })

    if (line.line !== null) drawSrcLine()
    jQuery.each(line.opcodes,drawOpCode)

    // Draw the main srouce line
    function drawSrcLine() {

      var skel = lineSkeleton('galua-source-line')
      var num  = skel.num
      var text = skel.text


      if (isCurrent) skel.row.addClass('cur-line')
      skel.row.addClass(srcLineClass)
              .data('galua-line',line.line)

      addBrkPoint(num, breakNum !== 0)
      theLine = {}

      var lineLab = $('<span/>')
                    .addClass('galua-line-number')
                    .text(' ' + line.line)


      if (line.opcodes.length > 0) {
        var menu = lineMenu(lineLab,toggleBreakOnSourceLine,
                              editBreakOnSourceLine,gotoSourceLine)
        num.append(menu)
        text
        .css('cursor','pointer')
        .click(function() {
          $('.' + mkClass(line.line)).slideToggle()
        })
      } else num.append(lineLab)


      jQuery.each(line.text, function(ix,t) {
        var it = $('<span/>')
                 .text(t.lexeme)
                 .addClass(t.token)
                 .addClass('galua-token ' + t.token)

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
            it.attr('id',t.name.ref)
            it.data('galua-pc',context.pc)
            it.addClass(t.name.active ? 'active' : 'inactive')
            it.hover(function() {
              $('.exp' + t.name.ref).addClass('gal_highlight_name')
            }, function() {
              $('.exp' + t.name.ref).removeClass('gal_highlight_name')
            })

            it.click(function () {
              if (it.hasClass('inactive')) return true
              jQuery.post('/watchName', { eid: context.eid
                                        , pc: it.data('galua-pc')
                                        , id: t.name.ref
                                        }, renderResult)
              .fail(disconnected)
              $('#value-pane').openModal()
              return false
            })
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
              if (x.type !== null) {
                h.append([ $('<br/>')
                         , $('<span/>')
                           .addClass('code_line identifier galua_remark')
                           .text(x.type.text)])
              }

              c.append(drawValueOpts(dbgState,x.value
                      ,dbgState.valueOpts({expand:true})))
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


    // Editing the condition on a source line always refers
    // to the first op-code on the line.
    function editBreakOnSourceLine() {
     if (line.opcodes.length === 0) return
     var op = line.opcodes[0]
     editBreakOnOpCode(0)
    }


    // When we click a source line we either:
    //  1. Remove all breakpoints (if any), or
    //  2. Add a break point to the first instruction.
    function toggleBreakOnSourceLine() {

      var todo = []
      jQuery.each(line.opcodes, function(i,o) {
        if (dbgState.breakPoints[opCodeKey(o)] !== undefined) todo.push(i)
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


    // Draw a code-viewer line
    function drawOpCode(i,op) {
      var skel = lineSkeleton('galua-opcode-line')
      var num  = skel.num

      if (line.line !== null) skel.row.addClass(mkClass(line.line))


      var openCur = $('#step-by-opcode').is(':checked')
      if (!(isCurrent && openCur)) skel.row.hide()

      var key     = opCodeKey(op)
      addBrkPoint(num,dbgState.breakPoints[key] !== undefined)

      subs[i]     = { id: key, row: skel.row }
      skel.row.addClass(key)

      if (pcKey === key) skel.row.addClass('cur-line')

      var lineLab = $('<span/>')
                    .addClass('galua-line-number')
                    .text(' ' + op.opcode)
      var menu = lineMenu( lineLab
                         , function() { toggleBreakOnOpCode(i, function() {}) }
                         , function() { editBreakOnOpCode(i) }
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
      var id      = subs[i].id
      var hasBrk  = (dbgState.breakPoints[id] !== undefined) ? true : false

      // Server interaction went OK
      function ok(res) {

        setClass($('.' + id + ' .breakpoint'), 'hide', hasBrk)
        hasBrk = !hasBrk
        if (hasBrk) dbgState.breakPoints[id] = null
        else dbgState.breakPoints[id] = undefined

        if (theLine !== null) {
            var lineHasBrk = false
            jQuery.each($('.' + mkClass(line.line + ' .breakpoint')),
                function(ix,it) {
                  if (!$(it).hasClass('hide')) {
                    lineHasBrk = true
                    return false // stop
                  }
                  return true // keep going
                })

            var brks = $('.' + srcLineClass + ' .breakpoint')
            setClass(brks,'hide', !lineHasBrk)

        }

        if (hasBrk) {
          $('#breakpoint-list').append(drawBreakPoint(dbgState,res))
        } else {
          $('.brk_menu.' + id).remove()
        }

        k()
      }

      jQuery.post( hasBrk ? '/removeBreakPoint' : '/addBreakPoint'
                 , { loc: id }, ok)
            .fail(disconnected)

    }

    function editBreakOnOpCode(i) {
      var id = subs[i].id
      var cond = dbgState.breakPoints[id]
      var hasBrk = cond !== undefined
      var hasCond = cond !== null
      editExpression('Break point condition'
                    , hasCond ? cond : ' '
                    , onDone)

      function onDone(expr) {
        var newCond = jQuery.trim(expr)

        jQuery.post('/addCondition', { loc: id, cond: newCond }, ok)
              .fail(disconnected) // XXX: Or syntax error?

        // This happens on successs
        function ok(res) {
          // Remeber the new condition
          dbgState.breakPoints[id] = (newCond === '' ? null : newCond)

          // Add the break point to the list; first we remove it,
          // then we redraw it.  This is because the list contains the
          // conditoin
          $('.brk_menu.' + id).remove()
          $('#breakpoint-list').append(drawBreakPoint(dbgState,res))

          // if this is not a new break point we are done.
          if (hasBrk) return

          // Otherwise we need to:

          //  1. add break point to the op-code
          setClass($('.' + id + ' .breakpoint'), 'hide', false)


          //  2. reveal the break point on the source line
          if (theLine !== null) {
            var brks = $('.' + srcLineClass + ' .breakpoint')
            setClass(brks,'hide', false)
          }

        }
      }
    }
  }
}






