

function drawNewThread(dbgState, thread) {

  var stack     = $('<ul/>').addClass('uk-list uk-list-line')
  var handlers  = $('<ul/>').addClass('uk-list uk-list-line')
  var locals    = $('<li/>')
  var code      = $('<table/>').addClass('code_box')

  var locals_entry    = locals
  var stack_entry     = $('<li/>').append(stack)
  var handlers_entry  = $('<li/>').append(handlers)
  var code_entry      = $('<li/>').append(code)




  var tab = $('<a/>').attr('href','').text('Thread ' + thread.name)

  var closeBtn = $('<span/>')
                 .addClass('uk-close')
                 .css('margin-left', '0.5em')
                 .click(function() {
                          tab.remove()
                          locals_entry.remove()
                          stack_entry.remove()
                          handlers_entry.remove()
                          code_entry.remove()
                          return false })

  tab.append(closeBtn)


  $('#thread_vars').append(locals_entry)
  $('#thread_stack').append(stack_entry)
  $('#thread_handlers').append(handlers_entry)
  $('#thread_code').append(code_entry)

  $('#thread_tabs').append($('<li/>').append(tab))


  var threadParts = { stack: stack
                    , handlers: handlers, code: code, locals: locals }

  var env = thread.env

  fillInExecEnv(dbgState, threadParts, env)

  jQuery.each(thread.stack, function(ix,f) {
    var fst = ix === 0
    threadParts.stack.append(drawStackFrame(dbgState, threadParts, f, fst))
  })

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
  var box = $('<div/>').addClass('uk-alert')
  var box = $('<li/>')

  switch (f.tag) {
    case 'call':
      var ptr = $('<i/>').addClass('stack-ptr uk-icon-chevron-right')
                         .css('margin-right', '0.5em')
      if (!focused) ptr.addClass('uk-hidden')
      box.append(ptr)
      box.append(drawFunName(f))
      box.css('cursor','pointer')
      box.click(function() {
          jQuery.post('/expand', { id: f.ref }, function(exp) {
            fillInExecEnv(dbgState,threadParts, exp)
            box.siblings().children()
                          .filter('.stack-ptr')
                          .addClass('uk-hidden')
            ptr.removeClass('uk-hidden')
          }).fail(disconnected)
      })
      break

    case 'throw_error':
      box.addClass('uk-alert-danger').text('Error')
  }
  return box
}


function drawHandler(dbgState, h) {
  var me = $('<li/>')
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


