function drawFunNameToolTip(obj) {
  if (!obj.file && !obj.line) return '(unknown)'
  var file = obj.file? obj.file         : ''
  var line = obj.line? (':' + obj.line) : ''
  return file + line
}

function drawValue(dbgState,v) {
        return drawValueEx(dbgState,v,false);
}

function drawValueEx(dbgState,v,startExpanded) {

  var label

  switch (v.tag) {
    case 'table':
    case 'user_data':
    case 'thread':
      var icons = { 'table':      'table'
                  , 'user_data':  'user'
                  , 'thread':     'random'
                  }
      label = $('<div/>')
              .append( $('<i/>').addClass('uk-icon-' + icons[v.tag]) )

      if (v.name !== undefined)
        label.append($('<span/>').addClass('code_line uk-text-bold')
                                 .text(' ' + v.name))

      if (v.tag === 'thread')
        label.append( $('<sup/>')
                      .addClass('uk-text-small uk-text-muted')
                      .text(v.status))

      label.append( $('<span/>').addClass('code-line')
                                .addClass('uk-text-small uk-text-muted')
                                .text(' ' + v.text) )

      break


    case 'closure':
      label = $('<span/>')
              .append([ $('<i/>').addClass('uk-icon-laptop')
                      , $('<sup/>').addClass('uk-text-small uk-text-muted')
                                   .text(v.type)
                      ])

      if (v.name !== undefined) {
        var nm = $('<span/>').addClass('code_line uk-text-bold')
                             .text(' ' + v.name)
        nm.attr('title', drawFunNameToolTip(v))
        label.append(nm)
      }

     label.append($('<span/>').addClass('code-line uk-text-small uk-text-muted')
                              .text(' ' + v.text))
     break

    case 'string':
      label = $('<div/>')
              .addClass('code_line literal')
              .css('word-wrap','break-word')
              .css('max-width','30em')
              .text(v.text)
      break

    case 'number':
    case 'light_user_data':
      label = $('<span/>').addClass('code_line literal').text(v.text)
      break

    case 'bool':
    case 'nil':
      label = $('<span/>').addClass('code_line keyword').text(v.text)
      break

    default:
      label = $('<span/>').addClass('value').text(v.text)
  }

  var x = drawCollapsedEx(dbgState, label, v, startExpanded)
  return $('<span/>').append(x.small)
                     .append(x.big)
}


function drawOrigin(o) {
  var lab = $('<i/>')
           .addClass('uk-icon-question uk-icon-hover uk-icon-small')
           .attr('style','padding: 0.1em')
           .attr('title', 'Origin')
           .attr('data-uk-tooltip','')
           .css('cursor', 'pointer')

  var info = $('<div/>').text('Defined by ')


  switch (o.type) {

    case '(system)':
      info.append($('<span/>').text('the system.'))
      break

    case 'Lua':
      info.append(drawFunName(o))
      break

    case 'C':
      info.append(drawFunName(o))
      break
  }

  info.hide()
  lab.click(function() { info.toggle() })


  return { icon: lab, dom: info }

}


function drawExpandCollapseIcon(dbgState, v, startExpanded) {

  var downloaded = false
  var open       = startExpanded

  var initialImage = open ? 'uk-icon-caret-up' : 'uk-icon-caret-down'

  var expandIcon = $('<i/>')
      .addClass(initialImage + ' uk-icon-hover uk-icon-small')
      .attr('style','padding: 0.1em')
      .attr('title', 'Expand')
      .attr('data-uk-tooltip','')
      .css('cursor', 'pointer')

  var here = $('<div/>')

  if (!open) {
          here.hide()
  }

  function showIt() {
        open = true
        here.slideDown()
        expandIcon.removeClass('uk-icon-caret-down')
                  .addClass('uk-icon-caret-up')
                  .attr('title', 'Collapse')
  }

  function hideIt() {
        open = false
        here.slideUp()
        expandIcon.removeClass('uk-icon-caret-up')
                  .addClass('uk-icon-caret-down')
                  .attr('title', 'Expand')
  }

  function doOpen() {
          jQuery.post('/expand', { id: v.ref }, function(exp) {
             here.append(renderExpanded(dbgState, exp))
             showIt()
             downloaded = true
         }).fail(disconnected)
  }

  if (open) { doOpen() }

  expandIcon.click(function() {
       if (open) {
         hideIt()
       } else {
         if (!downloaded) {
           doOpen()
         } else {
           showIt()
         }
       }
    })

  return { icon: expandIcon, dom: here }
}


function drawAddWatchIcon(v) {
  var watchIcon = $('<i/>')
        .addClass('uk-icon-eye uk-icon-small uk-icon-hover')
        .attr('style','padding: 0.1em')
        .css('cursor', 'pointer')
        .attr('title', 'Start monitoring')
        .attr('data-uk-tooltip','')

        .click( function () {
          jQuery.post('/watch', { id: v.ref }, function(exp) {
             console.log(exp)
          })
          .fail(disconnected)
        })

  return watchIcon
}

function drawAltRepIcon(v,lab,alt) {
  var icon = $('<i/>')
        .addClass('uk-icon-rotate-right uk-icon-small uk-icon-hover')
        .attr('style','padding: 0.1em')
        .css('cursor', 'pointer')
        .attr('title', 'Change representation')
        .attr('data-uk-tooltip','')

        .click( function () {
           var tmp = lab.text()
           lab.text(alt)
           alt = tmp
        })

  return icon
}


function drawViewFunctionIcon(dbgState,v) {
  return $('<i/>')
         .addClass('uk-icon-laptop uk-icon-small uk-icon-hover')
         .attr('style','padding: 0.1em')
         .css('cursor', 'pointer')
         .attr('title', 'View source')
         .attr('data-uk-tooltip','')
         .click( function () {
           var path = '/function'
           jQuery.post(path, { fid: v.fid }, function(code) {
             drawFunctionInNewTab(dbgState,code)
           })
           .fail(disconnected)
        })
}



function drawViewThreadIcon(dbgState,v) {
  return $('<i/>')
         .addClass('uk-icon-random uk-icon-small uk-icon-hover')
         .attr('style','padding: 0.1em')
         .css('cursor', 'pointer')
         .attr('title', 'View thread')
         .attr('data-uk-tooltip','')
         .click( function () {

           jQuery.post('/expand', { id: v.ref }, function(exp) {
             drawNewThread(dbgState,exp)
           })
           .fail(disconnected)
        })
}




function drawSetValueIcon(dbgState,v) {
  function makeTextBox(){
    var txt = $('<input/>').attr('type','text')
    txt.keypress(function(e) {
      if (e.which && e.which === 13 ||
          e.keyCode && e.keyCode === 13) {
        var newVal = txt.val()
        txt.remove()
        jQuery.post('/setValue', { id: v.ref, value: newVal })
              .fail(disconnected)
      }
    })
    return txt
  }

  var me = $('<i/>')
           .addClass('uk-icon-wrench uk-icon-small uk-icon-hover')
           .attr('style','padding: 0.1em')
           .css('cursor', 'pointer')
           .attr('title', 'Change value')
           .attr('data-uk-tooltip','')
           .click( function () {
              me.parent().append(makeTextBox())
           })
  return me
}

function drawAnalyzeIcon(dbgState, v) {
  var me = $('<i/>')
           .addClass('uk-icon-cubes uk-icon-small uk-icon-hover')
           .attr('style','padding: 0.1em')
           .css('cursor', 'pointer')
           .attr('title', 'Analyze')
           .attr('data-uk-tooltip','')

  var dom = $('<div/>').addClass('uk-panel uk-panel-box').hide()

  var closeBtn = $('<i>')
                 .addClass('uk-icon-close uk-icon-hover')
                 .click(function() { dom.empty().hide() })

  me.click( function () {
    jQuery.post('/analyze', { id: v.ref }, function(res) {
      console.log(res)
      dom.empty().append([closeBtn,aResult(res)]).show()
    })
    .fail(disconnected)
  })


  return { icon: me, dom: dom }
}


function drawCollapsed(dbgState, lab, v) {
        return drawCollapsedEx(dbgState, lab, v, false);
}

// A thing with a little down arrow, to get more info
function drawCollapsedEx(dbgState, lab, v, startExpanded) {

  var icons = []
  var here  = []



  // Expandable icon
  switch(v.tag) {
    case 'table':
    case 'user_data':
    case 'closure':
      var expand = drawExpandCollapseIcon(dbgState, v, startExpanded)
      icons.push(expand.icon)
      here.push(expand.dom)
  }

  // Origin icon
  switch(v.tag) {
    case 'table':
    case 'closure':
    case 'user_data':
      if (v.origin) {
        var orig = drawOrigin(v.origin)
        icons.push(orig.icon)
        here.unshift(orig.dom)
      }
  }



  // View thread icon
  switch(v.tag) {
    case 'thread':
      icons.push(drawViewThreadIcon(dbgState,v))
  }

  // View function icon
  switch(v.tag) {
    case 'closure':
      if (v.type === "Lua")
        icons.push(drawViewFunctionIcon(dbgState,v))
  }


  // Watch icon
  icons.push(drawAddWatchIcon(v))

  // Analyze icon
  switch(v.tag) {
    case 'closure':
      var res = drawAnalyzeIcon(dbgState,v)
      icons.push(res.icon)
      here.unshift(res.dom)
  }



  // Alternative representation icon
  switch(v.tag) {
    case 'string':
      icons.push(drawAltRepIcon(v, lab, v.alt))
      break;
  }

  icons.push(drawSetValueIcon(dbgState,v))




  var menu = $('<div/>')
             .addClass('uk-dropdown uk-dropdown-small min-width-auto')
             .append(icons)

  var small = $('<span/>')
               .attr('data-uk-dropdown','{pos:"right-center"}')
               .addClass('uk-button-dropdown')
               .append([lab,menu])

  return { small: small, big: here }
}



// Additional information about a value
function renderExpanded(dbgState, v) {

  switch (v.tag) {
    case 'table': return drawTable(dbgState,v)
    case 'user_data': return drawUserData(dbgState,v)

    case 'lua-fun':
      return drawLuaClosure(dbgState,v)

    case 'c-fun':
      return drawValueList(dbgState,'ups', v.upvalues)

    case 'prim-fun': return drawHsClosure(dbgState,v)

    case 'thread':
      drawNewThread(dbgState,v)
  }
}




// Expanded primitive closure
function drawHsClosure(dbgState,x) {
  var dom = $('<div/>')
  jQuery.each(x.args, function(i,a) { dom.append(drawPrimArg(a)) })

  dom.append(drawValueList(dbgState,'ups', x.upvalues))
  return dom
}


function drawPrimArg(x) {
  return $('<span/>')
         .addClass('literal')
         .css('padding-left', '0.5em')
         .attr('title', x.tag)
         .attr('data-uk-tooltip','')
         .text(x.text)
}



// Expanded Lua closure
function drawLuaClosure(dbgState, t) {
  var uvs = drawValueList(dbgState,'ups',t.upvalues)
  return $('<div/>').append(uvs)
}




// Expanded user data
function drawUserData(dbgState,t) {
  var d     = $('<table/>').addClass('uk-table uk-table-condensed')
  var row   = $('<tr/>')
  var right = $('<td/>')

  var bigR  = $('<tr/>')
  var bigC  = $('<td/>')
  bigR.append(bigC)


    console.log(t)
  if (t.ref !== undefined) {
    var meta = drawCollapsed(dbgState
                            ,$('<div/>')
                            .text('meta ' + t.text)
                            .addClass('code-line uk-text-small uk-text-muted')
                            , t
                            )
    right.append(meta.small)
    bigC.append(meta.big)
  }

  d.append([row,bigR])
  row.append([right])
  return d
}



// Expanded table
function drawTable(dbgState,t) {

  var d = $('<table/>')
          .addClass('uk-table uk-table-condensed uk-table-striped')
          // .addClass('value-list-table')

  jQuery.each(t.values, function(ix,entry) {
    var r = $('<tr/>')
    var k = $('<td/>').append(drawValue(dbgState,entry.key))
    var v = $('<td/>').append(drawValue(dbgState,entry.value))
    d.append(r)
    r.append([k,v])
  })

  if (t.ref !== undefined) {
    var r = $('<tr/>')
    var meta = drawCollapsed(dbgState
                            ,$('<span/>')
                            .text('meta ' + t.text)
                            .addClass('code-line uk-text-small uk-text-muted')
                            , t
                            )
    r.append($('<td/>').attr('colspan','2').append(meta.small))

    var r2 = $('<tr/>')
    r2.append($('<td/>').attr('colspan','2').append(meta.big))

    d.append([r,r2])
  }

  return d

}

