

function drawNewThread(dbgState, thread) {

  var threadId = "thread_" + thread.name
  var codeID   = "code_for_" + thread.name

  function allocateNewParts() {
    var stack     = $('<ul/>').addClass('stack collection')
    var handlers  = $('<ul/>').addClass('handlers collection')
    var locals    = $('<ul/>').addClass('variables collection')
    var code = newCodeContainer(dbgState,'Thread ' + thread.name, codeID, null)

    var stacks = $('<div/>').addClass('row')
                 .append([ $('<div/>').addClass('col s6').append(stack)
                         , $('<div/>').addClass('col s6').append(handlers)
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
               .append([ entry('Variables', true, locals)
                       , entry('Stacks', false, stacks)
                       ])
               .collapsible()

    var tabs = $('#thread_tabs')
    $('#thread_panes').append(pane)

    tabs.append($('<li/>')
                .addClass('tab')
                .append($('<a/>')
                        .attr('href', '#' + threadId)
                        .text('Thread ' + thread.name)))
    tabs.tabs({onShow: function(x) { console.log(x) }})
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
                    }

  var env = thread.env

  fillInExecEnv(dbgState, threadParts, env)

  threadParts.stack.empty()
  jQuery.each(thread.stack, function(ix,f) {
    var fst = ix === 0
    threadParts.stack.append(drawStackFrame(dbgState, threadParts, f, fst))
  })

  threadParts.handlers.empty()
  jQuery.each(thread.handlers, function(ix,h) {
    threadParts.handlers.append(drawHandler(dbgState, h))
  })

}





function fillInExecEnv(dbgState, threadParts, env) {

  threadParts.locals.empty()
                    .append([ drawFunName(env)
                            , $('<hr/>')
                            , drawValueList(dbgState, 'ups',  env.upvalues)
                            , drawValueList(dbgState, 'vas',  env.varargs)
                            , drawValueList(dbgState, 'regs', env.registers)
                            ])

  threadParts.code.empty()

  if (env.code) {
    var oldPC = dbgState.programCounter
    if (env.fid && env.pc !== undefined)
       dbgState.programCounter = { pc: env.pc, fid: env.fid }
    else {
       dbgState.programCounter = null
    }
    drawFunction(dbgState, threadParts.code, env.code)
    dbgState.programCounter = oldPC
  }

}


// The `threadParts` are passed in so that we can fill in the correct
// elements when the user asks for details of the stack frame.
function drawStackFrame(dbgState,threadParts,f,focused) {
  var box = $('<li/>').addClass('collection-item')

  switch (f.tag) {
    case 'call':
      var ptr = $('<span/>')
                .addClass('secondary-content stack-ptr')
                .append($('<i/>').addClass('material-icons')
                                 .text('chevron_left'))
      if (!focused) ptr.addClass('hide')
      box.append(ptr)
      box.append(drawFunName(f))
      box.css('cursor','pointer')
      box.click(function() {
          jQuery.post('/expand', { id: f.ref }, function(exp) {
            fillInExecEnv(dbgState,threadParts, exp)
            box.siblings().children()
                          .filter('.stack-ptr')
                          .addClass('hide')
            ptr.removeClass('hide')
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
      me.text('(default)')
      break
    case 'handler':
      me.append(drawValue(dbgState,h.value))
      break
    default:
      console.log('Unexpected handler', h)
  }
  return me
}

