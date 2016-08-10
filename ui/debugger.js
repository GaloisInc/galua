function doPoll(cmd) {
  jQuery.post('/poll', { timeout: 10, command: cmd },
    function(res) {
      if (cmd !== res)
        jQuery.get('/view', drawDebugger()).fail(disconnected)
      window.setTimeout(doPoll, 0, res)
    })
}

function disconnected(info,x ) {
  console.log(info,x)

  if (info.status === 423)
    setStatus('uk-badge-warning', 'Busy')
  else
    setStatus('uk-badge-danger', 'Error (' + info.status + ')')
  return false
}


function setBreakOnError() {
  var me = $('#break_on_error')
  var on = me.is(':checked')
  jQuery.post('/breakOnErr', { enabled: on}
             , function() { me.prop('checked',on) })
  return false
}


function newDebuggerState(d) {
  var brks = {}
  var errs = {}
  var pc   = null

  if (d.state.tag === 'running')
    pc = { fid: d.state.vm.thread.env.fid, pc: d.state.pc }

  jQuery.each(d.breakPoints, function(ix,loc) { brks[loc.id] = true })

  return { breakPoints: brks, programCounter: pc }
}

function setStatus(ty,msg) {
           $('#status').removeClass('uk-badge-success')
                       .removeClass('uk-badge-danger')
                       .removeClass('uk-badge-warning')
                       .addClass(ty)
                       .empty()
                       .text(msg)
}


function makeStep(how) {
  var byLine = $('input[name=step_type]:checked').val() === 'Line'
  return drawStep(how + (byLine ? 'Line' : ''), !byLine)
}


function drawStep(meth) {
  jQuery.post(meth,{}).fail(disconnected)
  if (meth === 'run')
    jQuery.post('/view',{},drawDebugger()).fail(disconnected)
}

function clearBreakPoints() {
  jQuery.post('/clearBreakPoints',
     function() {
       var list = $('#breakpoints-list')
       list.empty()
     }).fail(disconnected)
}

function drawPrints(prints, here) {

  var con = $('<ul/>').addClass('uk-list uk-list-space')
  jQuery.each(prints, function(ix,ln) {
    var it = $('<li/>')
    if (ln.num !== 1)
      it.append($('<span/>')
                .addClass('uk-badge uk-badge-notification')
                .text(ln.num))

    jQuery.each(ln.words, function(ix,w) {
      it.append($('<span/>').addClass('gal_code_box code_line').text(w))
    })
    con.append(it)
  })
  here.append(con)

}

function drawFailedToConnect(msg) {
  return $('<div/>')
         .attr('data-uk-alert','')
         .addClass('uk-alert uk-alert-danger')
         .append([ $('<a/>').addClass('uk-alert-close uk-close')
                 , $('<p/>').text(msg)
                 ])
}

function drawBreakPoint(dbgState,brk) {
  var nm = $('<span/>').text(brk.name)
  if (brk.file !== null)
    nm.attr('title', brk.file)
      .attr('data-uk-tooltip','')

  var link = $('<a/>')
             .append( [ $('<span/>')
                        .addClass('uk-badge uk-margin-right')
                        .text(brk.line)
                      , nm ])
             .click(function() {
                UIkit.offcanvas.hide()
                jQuery.post('/function', { fid: brk.fid },
                    function(code) {
                      drawFunctionInNewTab(dbgState, code)
                    }).fail(disconnected)
              })

  var thing = $('<li/>')
              .addClass('brk_menu')
              .addClass(brk.id)
              .append(link)

  return thing
}

function drawBreakPoints(dbgState, breaks) {
  var list = $('#breakpoints-list')
  list.empty()
  jQuery.each(breaks,function(ix,brk) {
    list.append(drawBreakPoint(dbgState,brk))
  })
}

function drawSources(dbgState, sources) {
  var list    = $('#top-source-list').empty()
  visible = []
  var els = []

  jQuery.each(sources, function(ix,source) {
    els.push(drawNode(source))
  })
  visible[0] = els
  list.append(els)

  function drawSubMenu(icon,nm,els) {

    var item =
      $('<li/>')
      .append($('<a/>').attr('href','#')
                       .append( [ $('<i/>').addClass(icon)
                                , $('<span/>').text(' ' + nm)
                                ]))

    jQuery.each(els,function(ix,el) { el.hide() })

    var backLink = $('<a/>').attr('href','#').text('..')
    var back = $('<li/>').hide().append(backLink)

    var title = $('<span/>')
                .attr('title',nm)
                .attr('data-uk-tooltip','')
                .hide()
                .text(nm.length < 22 ? nm
                                     : ('... ' + nm.substr(-21))
                ).hide()
    $('#source-title').append(title)


    list.append(back)
    list.append(els)
    $('#source-title').append(title)

    els.push(back)
    els.push(title)
    // Now `els` has all the things in our menu.
    // Note that item belongs to the parent menu.

    function showAll (xs) { jQuery.each(xs,function(ix,x) { x.show() }) }
    function hideAll (xs) { jQuery.each(xs,function(ix,x) { x.hide() }) }

    item.click(function() {
      hideAll(visible[visible.length - 1])
      visible.push(els)
      showAll(els)
    })

    backLink.click(function() {
      hideAll(visible.pop())
      showAll(visible[visible.length - 1])
    })

    return item
  }



  function drawNode(node) {

    var nm = node.name === null ? '(no name)' : node.name

    switch(node.tag) {

      case 'path':
        var els = []
        jQuery.each(node.subs, function(ix,sub) { els.push(drawNode(sub)) })
        return drawSubMenu('uk-icon-folder', nm, els)

      case 'chunk':
        var els = []
        jQuery.each(node.funs, function(ix,f) {
          var link = $('<a/>').attr('href','#').text(f.name)
          els.push($('<li/>').append(link))
          link.click(function() {
            UIkit.offcanvas.hide()
              jQuery.post(
                '/function', { fid: f.id },
                function(code) {
                  drawFunctionInNewTab(dbgState, code)
                  UIkit.offcanvas.hide()
                }).fail(disconnected)
          })
        })

        return drawSubMenu('uk-icon-laptop', nm, els)

      default:
        console.log('Unknown tag for source: ', node)

        return []
    }
  }
}

function drawProfiling(dbgState,stats) {
  var list = $('#profiling-list')
  list.empty()
  var tab = $('<table/>').addClass('uk-text-contrast')

  tab.append($('<tr/>')
             .append([ $('<td/>').attr('colspan',4).text('Function')
                     , $('<th/>').text('Calls')
                     , $('<th/>').text('Indiv')
                     , $('<th/>').text('Cumul')
                     ]))
  jQuery.each(stats.calls, function(ix,entry) {
    var count = $('<td/>').text(entry.calls)
    var ind   = $('<td/>').text(entry.ind.toFixed(2))
    var cum   = $('<td/>').text(entry.cum.toFixed(2))
    var name  = $('<td/>')
    var ty    = $('<td/>').addClass('uk-badge')
    var pc    = $('<td/>')
    var args  = $('<td/>')
    drawFunParts(entry.loc, name, ty, pc, args)
    var row = $('<tr/>').append([ty,name,pc,args,count,ind,cum])
    tab.append(row)

    if (entry.loc.type === 'Lua') {
      row.css('cursor','pointer') // XXX: Is there a class for this?
         .click(function() {
         jQuery.post('/function', { fid: entry.loc.fid },
                     function(code) {
                       drawFunctionInNewTab(dbgState, code)
                     }).fail(disconnected)
         })
    }

  })



  list.append(tab)
}


function drawDebugger() { return function (d) {
  var body      = $('body')

  var status    = $('#status').removeClass('uk-badge-success')
                              .removeClass('uk-badge-danger')
  var prints    = $('#prints').empty()
  var result    = $('#result').empty()
  var mons      = $('#monitoring').empty()

  $('#code_pane_control>.dynamic').remove()
  $('#code_pane>.dynamic').remove()

  $('#break_on_error').prop('checked', d.breakOnError)

  function onDone(isDone) {
    if (isDone) $('.hide_on_finish').addClass('uk-hidden')
    else $('.hide_on_finish').removeClass('uk-hidden')
  }

  var dbgState = newDebuggerState(d)
  drawSources(dbgState, d.sources)
  drawBreakPoints(dbgState, d.breakPoints)


  drawPrints(d.prints, prints)

  var state = d.state

  switch (state.tag) {

    case 'running':
      onDone(false)
      setStatus('uk-badge', d.idle.tag)

      mons.append(drawValueList(dbgState, 'watches', d.watches))

      $('#thread_tabs').empty()
      $('#thread_vars').empty()
      $('#thread_stack').empty()
      $('#thread_handlers').empty()
      $('#thread_code').empty()

      drawProfiling(dbgState,state.vm.stats)

      drawNewThread(dbgState, state.vm.thread)
      var blocked = $('#blocked_threads').empty()
      jQuery.each(state.vm.blocked,function(ix,v) {
        blocked.append($('<li/>').append(drawValue(dbgState,v)))
      })

      UIkit.switcher($('#thread_tabs')).show(1)

      if (d.idle.error !== undefined) {
        var badge = $('<div/>')
                    .addClass('uk-badge uk-badge-danger uk-margin-right')
                    .text('error')

        result.append([badge, drawValue(dbgState,d.idle.error)])
      }

      focusCurLine()
      break

    case 'finished':
      onDone(true)
      setStatus('uk-badge-success', 'Finished')
      jQuery.each(state.result, function(ix,v) {
        result.append(drawValue(dbgState,v))
      })
      break

    case 'error':
      onDone(true)
      setStatus('uk-badge-danger', 'Error')
      result.append(drawValue(dbgState,state.error))
      break
  }

}}


function drawValueList(dbgState, which, vs) {

  var title = '?'
  var prefix = '?'
  switch(which) {
    case 'watches': title = null;       prefix = 'M'; break
    case 'regs':    title = 'Locals';   prefix = 'R'; break
    case 'ups':     title = 'Upvalues'; prefix = 'U'; break
    case 'vas':     title = 'Varargs';  prefix = '...'; break
  }

  var vals = $('<table/>').addClass('uk-table uk-table-condensed')
  if (title) vals.append($('<caption/>').text(title))

  var lastNonNil = -1
  jQuery.each(vs,function(ix,val) {
    if (val.name !== null || val.val.tag !== 'nil') {
      lastNonNil = ix;
    }
  });

  var subvs = $(vs).slice(0,lastNonNil+1);
  jQuery.each(vs,function(ix,val) {
    var altName = prefix + '[' + (ix+1) + ']'
    var row = $('<tr/>')
    vals.append(row)
    row.append($('<td/>')
      .attr('title',altName)
      .attr('data-uk-tooltip','{pos:\'top\'}')
      .text(val.name === null ? altName : val.name))
    row.append($('<td/>').append(drawValue(dbgState,val.val)))
  })

  return vals
}




function drawFunParts(f, nameHere, typeHere, pcHere, argsHere) {

  nameHere.empty()
  typeHere.empty()
  argsHere.empty()

  typeHere.text(f.type)
  nameHere.text(f.name)

  if (pcHere !== null) pcHere.empty()
  if (f.method !== undefined) {
      argsHere.append('- '+f.method+' (')
      if (f.args !== undefined) {
        var leader = ''
        jQuery.each(f.args, function(ix,a) {
            argsHere.append(leader)
            argsHere.append(drawPrimArg(a))
            leader = ', '
        })
      }
      argsHere.append(' )')
      if (f.return !== undefined) {
        if (f.return.file !== undefined) {
          var txt = f.return.file
          if (f.return.line !== undefined && f.return.line !== null) {
            txt = txt + ':' + f.return.line
          }
          nameHere.attr('title', txt)
                  .attr('data-uk-tooltip','')
        }
      }
  }


  switch (f.type) {
    case 'Lua':
      nameHere.attr('data-uk-tooltip','{pos:\'top\'}')
      nameHere.attr('title',f.file)
      if (pcHere !== null && f.pc !== undefined)
                           pcHere.addClass('uk-text-small uk-text-muted')
                                 .text('opcode: ' + f.pc)
      break

    case 'C':
      if (f.file !== null) {
        nameHere.attr('data-uk-tooltip','{pos:\'top\'}')
        nameHere.attr('title',f.file)
      }
      break
  }
}


function drawFunName(f) {

  var ty    = $('<span/>').addClass('uk-badge')
  var name  = $('<span/>')
  var pc    = $('<span/>')
  var args  = $('<span/>')

  drawFunParts(f, name, ty, pc, args)
  return $('<span/>').append([ty,' ',name,' ',args, ' ', pc])
}


