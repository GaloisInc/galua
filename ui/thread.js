
function listThreads() {
  var names = []
  jQuery.each($('#thread_tabs>li>a'), function(ix,el) {
    names.push($(el).attr('href').replace('#thread_',''))
  })
  return names
}

function removeThread(thread_name) {
  var tid = 'thread_' + thread_name
  var cid = 'code_for_' + thread_name

  function rm(loc,nm) {
    $('#' + loc + '_tabs>li:has(a[href="#' + nm + '"])').remove()
    $('#' + loc + '_panes>#' + nm).remove()
    $('#' + loc + '_tabs').tabs()
  }

  rm('thread', 'thread_' + thread_name)
  rm('code','code_for_' + thread_name)
}



function drawNewThread(dbgState, thread) {

  var threadId = "thread_" + thread.name
  var codeID   = "code_for_" + thread.name

  function allocateNewParts() {
    var stack     = $('<ul/>').addClass('stack collection')
    var handlers  = $('<ul/>').addClass('handlers collection')
    var locals    = $('<ul/>').addClass('variables collection')
    var code = newCodeContainer(dbgState,'Thread ' + thread.name, codeID, null)


    var varSec =
      $('<li/>')
      .append([ $('<div/>')
                .addClass('collapsible-header active')
                .append([ $('<span/>')
                          .addClass('galua_section_label').text('Variable')
                        , $('<span/>').addClass('curFun')
                        ]
                       )
              , $('<div/>')
              .addClass('collapsible-body')
              .append(locals)
              ])

    function entry(name,active,thing) {
      return $('<li/>')
             .append([ $('<div/>')
                       .addClass('collapsible-header'+(active ? ' active' : ''))
                       .append($('<span/>')
                               .addClass('galua_section_label').text(name))
                     , $('<div/>')
                       .addClass('collapsible-body')
                       .append(thing) ])
    }

    var pane = $('<ul/>')
               .attr('id', threadId)
               .addClass('collapsible')
               .attr('data-collapsible','expandable')
               .append([ varSec // entry('Variables', true, locals)
                       , entry('Call Stack', false, stack)
                       , entry('Error Handlers', false, handlers)
                       ])
               .collapsible()

    var tabs = $('#thread_tabs')
    $('#thread_panes').append(pane)

    tabs.append($('<li/>')
                .addClass('tab')
                .append($('<a/>')
                        .attr('href', '#' + threadId)
                        .text('Thread ' + thread.name)))
    tabs.tabs()
    tabs.tabs('select_tab', threadId)
  }

  var me = $('#' + threadId)
  if (me.length === 0) {
    allocateNewParts()
    me = $('#' + threadId)
  }

  var out = $('#' + codeID)

  var threadParts = { stack:    me.find('.stack')
                    , handlers: me.find('.handlers')
                    , code:     out.find('.function')
                    , locals:   me.find('.variables')
                    , curFun:   me.find('.curFun')
                    }

  var env = thread.env

  fillInExecEnv(dbgState, threadParts, env)

  threadParts.stack.empty()
  jQuery.each(thread.stack, function(ix,f) {
    var fst = ix === 0
    threadParts.stack.append(drawStackFrame(dbgState, threadParts, ix, f, fst))
  })

  threadParts.handlers.empty()
  jQuery.each(thread.handlers, function(ix,h) {
    threadParts.handlers.append(drawHandler(dbgState, h))
  })

}





function fillInExecEnv(dbgState, threadParts, env) {

  threadParts.curFun.empty().append(drawFunName(env))


  threadParts.locals.empty()
                    .append([ drawValueList(dbgState, 'ups',  env.upvalues)
                            , drawValueList(dbgState, 'vas',  env.varargs)
                            , drawValueList(dbgState, 'regs', env.registers)
                            ])

  if (env.result) {
     var vals = $('<table/>').addClass('bordered')
     vals.append(
             [ $('<caption/>').text('Last C result'),
               $('<tr/>').append($('<td/>').append(drawPrimArg(env.result)))
             ])
     threadParts.locals.append(vals)
  }


  if (env.code) {
    var oldPC = dbgState.programCounter
    if (env.fid && env.pc !== undefined)
       dbgState.programCounter = { pc: env.pc, fid: env.fid }
    else {
       dbgState.programCounter = null
    }
    drawFunction(dbgState, threadParts.code, env.code)
    dbgState.programCounter = oldPC
  } else {
    var noCode = $('<div/>').text('No code available for this function')
    threadParts.code.empty().append(noCode)
               .data('galua-function',null)
  }

}


// The `threadParts` are passed in so that we can fill in the correct
// elements when the user asks for details of the stack frame.
function drawStackFrame(dbgState,threadParts,ix,f,focused) {
  var box = $('<li/>').addClass('collection-item')

  switch (f.tag) {
    case 'call':
      var ptr = $('<span/>')
                .addClass('secondary-content stack-ptr')
                .append($('<i/>').addClass('material-icons')
                                 .text('chevron_left'))
      var kbd = $('<i/>')
                .addClass('material-icons secondary-content stack-ptr')
                .text('keyboard')
                .click(function() {
                   editExpression('Evaluate Statment','',
                    function(txt) {
                      var args = { stat: txt, stackframe: f.ref }
                      jQuery.post('/exec', args, function() {}) // XXX: redraw
                    })
                 })
      if (!focused) { ptr.addClass('hide'); kbd.addClass('hide') }
      box.append([ptr,kbd])
      box.append(drawFunName(f))
      box.css('cursor','pointer')
      box.click(function() {

          dbgState.currentStackFrame = ix

          jQuery.post('/expand', { id: f.ref }, function(exp) {
            fillInExecEnv(dbgState,threadParts, exp)
            box.siblings().children()
                          .filter('.stack-ptr')
                          .addClass('hide')
            ptr.removeClass('hide')
            kbd.removeClass('hide')
          }).fail(disconnected)
      })
      break

    case 'throw_error':
      box.addClass('white-text red chip').text('Error')
  }
  return box
}


function drawHandler(dbgState, h) {
  var me = $('<li/>').addClass('collection-item')
  switch (h.tag) {
    case 'default':
      me.addClass('galua_remark').text('default handler')
      break
    case 'handler':
      me.append(drawValue(dbgState,h.value))
      break
    default:
      console.log('Unexpected handler', h)
  }
  return me
}


