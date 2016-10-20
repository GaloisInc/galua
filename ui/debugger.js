function doPoll(cmd) {
  jQuery.post('/poll', { timeout: 10, command: cmd },
    function(res) {
      if (cmd !== res) {
        jQuery.get('/view', drawDebugger()).fail(disconnected)
      } else {
        window.setTimeout(doPoll, 0, res)
      }
    })
}



function disconnected(info,x ) {
  console.log(info,x)

  if (info.status === 423)
    Materialize.toast('Busy', 5000)
  else
    setStatus('uk-badge-danger', 'Error (' + info.status + ')')
  return false
}


function setBreakOnError() {
  var me = $('#pause-on-error')
  var newSetting = !me.is(':checked')
  jQuery.post('/breakOnErr', { enabled: newSetting}
             , function() { me.prop('checked',newSetting) })
  return false
}


function newDebuggerState(d) {
  var brks = {}
  var errs = {}
  var pc   = null

  if (d.state.tag === 'running')
    pc = { fid: d.state.vm.thread.env.fid, pc: d.state.pc }

  jQuery.each(d.breakPoints, function(ix,loc) { brks[loc.id] = true })

  return { breakPoints:     brks
         , programCounter:  pc
         , fun_counter:     0

         // Read only
         , valueOpts: function(opts) {
              return jQuery.extend( { expanded:  true
                                    , watchMode:
                                      'watch' // | 'unwatch' | 'no-watch'
                                    }, opts)
           }
         }
}

function setStatus(ty,msg) {
  var cl

  switch(ty) {
    case 'uk-badge-success': cl = 'green';  break
    case 'uk-badge-warning': cl = 'yellow'; break
    case 'uk-badge-danger':  cl = 'red';   break
    default: cl = 'blue'
  }

  $('#status')
    .removeClass('blue green yellow red')
    .addClass(cl)
    .empty()
    .text(msg)
}

function drawPrints(dbgState,mbError, prints) {

  var here = $('#prints').empty()

  if (mbError !== undefined) {
    here.append($('<li/>')
                .addClass('collection-item')
                .append([ $('<span/>')
                          .addClass('secondary-content')
                          .append($('<i/>')
                                  .addClass('material-icons red-text')
                                  .text('error'))
                        , drawValue(dbgState,mbError)
                        ]))
  }

  if (prints.length === 0) {
    here.append($('<li/>').addClass('collection-item galua_remark')
                          .text('no output'))
  }
  else
    jQuery.each(prints, function(ix,ln) {
      var it = $('<li/>').addClass('collection-item')
      if (ln.num !== 1)
        it.append($('<span/>')
                  .addClass('blue white-text new badge')
                  .attr('data-badge-caption','times')
                  .text(ln.num))

      jQuery.each(ln.words, function(ix,w) {
        it.append($('<span/>').addClass('gal_code_box code_line').text(w))
      })
      here.append(it)
    })
}




function makeStep(how) {
  var byLine = !$('#step-by-opcode').is(':checked')
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
       var list = $('#breakpoint-list')
       list.empty()
       $('#breakpoint-pane').closeModal()
     }).fail(disconnected)
}

function drawBreakPoint(dbgState,brk) {
  return $('<a/>')
         .addClass('collection-item tooltipped brk_menu')
         .addClass(brk.id)
         .attr('data-tooltip', brk.file ? brk.file : '???')
         .append( [ brk.name
                  , $('<span/>')
                    .addClass('secondary-content')
                    .text('line ' + brk.line)
                  ] )
         .click(function() {
            $('#breakpoint-pane').closeModal()
            jQuery.post('/function', { fid: brk.fid },
              function(code) { drawFunctionInNewTab(dbgState, code) }
            ).fail(disconnected)
          })
          .tooltip()
}

function drawBreakPoints(dbgState, breaks) {
  var list = $('#breakpoint-list')
  list.empty()
  jQuery.each(breaks,function(ix,brk) {
    list.append(drawBreakPoint(dbgState,brk))
  })
}

function drawSources(dbgState, sources) {

  drawForest($('#function-browser-list').empty(), sources)

  function drawForest(prnt,xs) {

    jQuery.each(xs, function(ix,x) {
      var nm  = $('<div/>').addClass('collapsible-header')
      var bdy = $('<div/>').addClass('collapsible-body')

      switch(x.tag) {
        case 'chunk':
          nm.append([ $('<i/>').addClass('material-icons')
                               .text('insert_drive_file')
                    , $('<span/>').text(x.name)
                    ])

          var inner = $('<div/>').addClass('collection')
          jQuery.each(x.funs, function(ix,fun) {
            var link = $('<a/>')
                       .attr('href','#')
                       .addClass('collection-item').text(fun.name)
                       .click(function() {
                                 jQuery.post(
                                   '/function', { fid: fun.id },
                                   function(code) {
                                     drawFunctionInNewTab(dbgState, code)
                                     $('#function-pane').closeModal()
                                   }).fail(disconnected)
                               })
            inner.append(link)
          })

          bdy.append(inner)
          break

        case 'path':
          nm.append([ $('<i/>').addClass('material-icons').text('folder')
                    , $('<span/>').text(x.name)
                    ])

          var inner = $('<ul/>').addClass('collapsible')
                                .attr('data-collapsible','expandable')
                                .collapsible()
          drawForest(inner,x.subs)
          bdy.append(inner)
          break
      }
      prnt.append($('<li/>').append([nm,bdy]))
    })
  }


}


function drawProfiling(dbgState,stats) {
  var list = $('#profiling-list')
  list.empty()

  jQuery.each(stats.calls, function(ix,entry) {
    var count = $('<td/>').text(entry.calls)
    var ind   = $('<td/>').text(entry.ind.toFixed(2))
    var cum   = $('<td/>').text(entry.cum.toFixed(2))
    var name  = $('<td/>')
    var ty    = $('<td/>')
    var pc    = $('<td/>')
    var args  = $('<td/>')
    drawFunParts(entry.loc, name, ty, pc, args)
    var row = $('<tr/>').append([ty,name,pc,args,count,ind,cum])
    list.append(row)

    // XXX
    if (entry.loc.type === 'Lua') {
      row.css('cursor','pointer') // XXX: Is there a class for this?
         .click(function() {
         jQuery.post('/function', { fid: entry.loc.fid },
                     function(code) {
                       drawFunctionInNewTab(dbgState, code)
                       $('#profiling-pane').closeModal()
                     }).fail(disconnected)
         })
    }

  })
}

function drawRegistry( dbgState, val) {
  var opts = dbgState.valueOpts({ expand: true
                                , watchMode: 'no-watch'
                                })
  $('#registry-pane-content').empty()
                             .css('padding','15px')
                             .append(drawValueOpts(dbgState, val, opts))
}


function drawDebugger() { return function (d) {

  $('#pause-on-error').prop('checked', d.breakOnError)


  function onDone(isDone) {
    return
//XXX
    if (isDone) $('.hide_on_finish').addClass('uk-hidden')
    else $('.hide_on_finish').removeClass('uk-hidden')
  }

  var dbgState = newDebuggerState(d)
  drawSources(dbgState, d.sources)
  drawBreakPoints(dbgState, d.breakPoints)

  var state = d.state

  var mbError = undefined
  if (state.tag === 'error') mbError = state.error
  if (state.tag === 'running') mbError = d.idle.error
  drawPrints(dbgState,mbError,d.prints)

  switch (state.tag) {

    case 'running':
      onDone(false)
      setStatus('uk-badge', d.idle.tag)

      drawWatches(dbgState, d.watches)

      drawProfiling(dbgState, state.vm.stats)
      drawRegistry(dbgState,  state.vm.registry )

      drawNewThread(dbgState, state.vm.thread)
      jQuery.each(listThreads(),function(ix,t) {
        if (t === state.vm.thread.name) return true
        var tr = state.vm.openThreads[t]
        if (tr === undefined) {
          removeThread(t)
          return true
        }
        drawNewThread(dbgState, tr)
      })


      var blocked = $('#blocked_threads-content').empty()
      if (state.vm.blocked.length === 0) {
        blocked.append($('<li/>').addClass('galua_remark')
                                 .text('no blocked threads'))
      } else
        jQuery.each(state.vm.blocked,function(ix,v) {
          blocked.append($('<li/>')
                         .addClass('collection-item')
                         .append(drawValue(dbgState,v)))
        })

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
      // result.append(drawValue(dbgState,state.error))
      break
  }

  doPoll(d.stateCounter)
}}


function drawWatches(dbgState, vs) {

  var mons = $('#monitoring-content').empty()
  if (vs.length === 0) {
    mons.append(drawEmptyWatch())
    return
  }

  jQuery.each(vs,function(ix,val) {
    mons.append(drawWatched(dbgState,val))
  })

}

function drawEmptyWatch() {
  return $('<li/>')
         .attr('id','galua-empty-watch')
         .addClass('galua_remark')
         .text('not monitoring anything')

}

function drawWatched(dbgState,val) {
  var row = $('<li/>')
            .addClass('collection-item')
            .attr('id',watchId(val.id))
  row.append($('<span/>').addClass('badge').text(val.name))
  var opts = dbgState.valueOpts({watchMode: 'unwatch', watchId: val.id })
  row.append(drawValueOpts(dbgState,val.val,opts))
  return row
}

function watchId(i) { return 'watch_id_' + i }





function drawValueList(dbgState, which, vs) {

  var title = '?'
  var prefix = '?'
  switch(which) {
    case 'watches': title = null;       prefix = 'M'; break
    case 'regs':    title = 'Locals';   prefix = 'R'; break
    case 'ups':     title = 'Upvalues'; prefix = 'U'; break
    case 'vas':     title = 'Varargs';  prefix = '...'; break
  }

  title = title + ' (' + vs.length + ')'

  var vals = $('<table/>').addClass('bordered')
  if (title) vals.append($('<caption/>').text(title))

  var lastNonNil = -1
  jQuery.each(vs,function(ix,val) {
    if (val.name !== null || val.val.tag !== 'nil') {
      lastNonNil = ix;
    }
  });

  var subvs = $(vs).slice(0,lastNonNil+1);
  jQuery.each(subvs,function(ix,val) {
    var altName = prefix + '[' + (ix+1) + ']'
    var row = $('<tr/>')
    vals.append(row)
    var key = $('<td/>')
              .addClass('tooltipped')
              .attr('data-tooltip', val.name === null ? altName : val.name)
              .text(val.name === null ? altName : val.name)
    key.tooltip()
    row.append(key)
    row.append($('<td/>').append(drawValue(dbgState,val.val)))
  })

  return vals
}




function drawFunParts(f, nameHere, typeHere, pcHere, argsHere) {

  nameHere.empty()
  typeHere.empty()
  argsHere.empty()

  typeHere.append($('<span/>').addClass('galua_function_chip ' + f.type).text(f.type))
  nameHere.text(f.name)

  if (pcHere !== null) pcHere.empty()
  if (f.method !== undefined) {
      var methBox = $('<span/>')
                    .addClass('tooltipped')
                    .text(f.method)
                    .attr('data-tooltip', drawFunNameToolTip(f.return))
                    .tooltip()

      argsHere.append('- ')
              .append(methBox)
              .append(' (')
      if (f.args !== undefined) {
        var leader = ''
        jQuery.each(f.args, function(ix,a) {
            argsHere.append(leader)
            argsHere.append(drawPrimArg(a))
            leader = ', '
        })
      }
      argsHere.append(' )')

      if (f.phase !== undefined) {
          argsHere.append(' - ' + f.phase)
      }
  }

  nameHere.addClass('tooltipped')

  switch (f.type) {
    case 'Lua':
      nameHere.attr('data-tooltip',f.file).tooltip()
      if (pcHere !== null && f.pc !== undefined)
                           //XXX
                           pcHere.addClass('uk-text-small uk-text-muted')
                                 .text('opcode: ' + f.pc)
      break

    case 'C':
      if (f.file !== null) {
        nameHere.attr('data-tooltip',drawFunNameToolTip(f)).tooltip()
      }
      break
  }
}


function drawFunName(f) {
  var ty    = $('<span/>')
  var name  = $('<span/>')
  var pc    = $('<span/>')
  var args  = $('<span/>')

  drawFunParts(f, name, ty, pc, args)
  return $('<span/>').append([ty,' ',name,' ',args, ' ', pc])
}

function drawFunNameToolTip(obj) {
  if (!obj.file && !obj.line) return '(unknown)'
  var file = obj.file? obj.file         : ''
  var line = obj.line? (':' + obj.line) : ''
  return file + line
}



