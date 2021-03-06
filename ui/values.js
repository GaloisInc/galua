function drawValue(dbgState,v) {
  return drawValueOpts(dbgState,v,dbgState.valueOpts())
}

function drawValueOpts(dbgState,v,opts) {

  var label

  switch (v.tag) {
    case 'table':
    case 'user_data':
    case 'thread':
      var icons = { 'table':      'web'
                  , 'user_data':  'extension'
                  , 'thread':     'shuffle'
                  }
      label = $('<div/>')
              .css('display','inline-block')
              .append( icon('tiny',icons[v.tag]) )

      if (v.name !== undefined)
        label.append($('<span/>').addClass('code_line galua-small-label')
                                 .text(' ' + v.name))

      if (v.tag === 'thread')
        label.append( $('<sup/>')
                      .addClass('galua-small-label')
                      .text(v.status))

      label.append( $('<span/>').addClass('code-line galua-small-label')
                                .text(' ' + v.text) )

      break


    case 'closure':
      label = $('<span/>')
              .append([ icon('tiny', 'computer')
                      , $('<sup/>').addClass('galua-small-label')
                                   .text(v.type)
                      ])

      if (v.name !== undefined) {
        var nm = $('<span/>').addClass('code_line tooltipped')
                             .text(' ' + v.name)
        nm.attr('data-tooltip', drawFunNameToolTip(v))
        nm.tooltip()
        label.append(nm)
      }

     label.append($('<span/>').addClass('code-line galua-small-label')
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

  var x = drawCollapsedEx(dbgState, label, v, opts)
  return $('<span/>').append(x.small)
                     .append(x.big)
}


function drawOrigin(o) {
  var lab = roundButton('black white-text','blur_on', 'Defined by', handler)

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
  function handler() { info.toggle() }

  return { icon: lab, dom: info }

}


function drawExpandCollapseIcon(dbgState, v, opts) {

  var downloaded = false
  var open       = opts.expand

  var initialImage = open ? 'arrow_drop_up' : 'arrow_drop_down'

  var expandIcon = roundButton('black white-text'
                              ,initialImage,'Expand/collapse', handler)


  var here = $('<div/>')

  if (!open) {
          here.hide()
  }

  function showIt() {
        open = true
        here.slideDown()
        expandIcon.find('i').text('arrow_drop_up')
  }

  function hideIt() {
        open = false
        here.slideUp()
        expandIcon.find('i').text('arrow_drop_down')
  }

  function doOpen() {
          jQuery.post('/expand', { id: v.ref }, function(exp) {
             here.append(renderExpanded(dbgState, v.ref, exp))
             showIt()
             downloaded = true
         }).fail(disconnected)
  }

  if (open) { doOpen() }

  function handler() {
       if (open) {
         hideIt()
       } else {
         if (!downloaded) {
           doOpen()
         } else {
           showIt()
         }
       }
    }


  return { icon: expandIcon, dom: here }
}


function drawAddWatchIcon(dbgState,v,opts) {

  switch (opts.watchMode) {
    case 'watch':
      return roundButton('black white-text'
                        , 'visibility'
                        , 'Start monitoring'
                        , function () {
                            jQuery.post('/watch', { id: v.ref }, function(exp) {
                               $('#galua-empty-watch').remove()
                               $('#monitoring-content')
                               .append(drawWatched(dbgState,exp))
                            })
                            .fail(disconnected)
                        })
    case 'unwatch':
      return roundButton( 'black white-text'
                        , 'visibility_off'
                        , 'Stop monitoring'
                        , function () {
                            var i = opts.watchId
                            jQuery.post('/unwatch', { id: i },
                              function() {
                                $('#' + watchId(i)).remove()
                                var mons = $('#monitoring-content')
                                if (mons.children().length === 0)
                                   mons.append(drawEmptyWatch())
                              })
                            .fail(disconnected)
                        })
    default:
      console.log('Malformed `watchMode`', opts)
      return []
  }
}

function drawAltRepIcon(v,lab,alt) {
  var icon =
    roundButton( 'black white-text'
               , 'repeat'
               , 'Change representation'
               , function () { var tmp = lab.text()
                               lab.text(alt)
                               alt = tmp
                             })

  return icon
}


function drawViewFunctionIcon(dbgState,v) {
  return roundButton (
      'black white-text'
    , 'computer'
    , 'View source'
    , function () {
        var path = '/closure'
        jQuery.post(path, { fid: v.fid, id: v.id }, function(code) {
            drawFunctionInNewTab(dbgState,code)
        })
        .fail(disconnected)
    })
}



function drawViewThreadIcon(dbgState,v) {
  return roundButton ( 'black white-text'
                     , 'shuffle'
                     , 'View thread'
                     , function () {
                         jQuery.post('/expand', { id: v.ref }, function(exp) {
                              drawNewThread(dbgState,exp)
                         })
                         .fail(disconnected)
                       })
}




function drawSetValueIcon(dbgState,lab,v,opts) {

  return roundButton (
      'black white-text'
    , 'mode_edit'
    , 'Change value'
    , function() {
        editExpression('Set value', v.text, function(txt) {
          jQuery.post( '/setValue'
                     , { id: v.ref, value: jQuery.trim(txt) }
                     , function(newV) {
                          lab.replaceWith(drawValueOpts(dbgState,newV,opts))
                       }
                     )
                .fail(disconnected)
        })
      }
    )

}

function drawAnalyzeIcon(dbgState, v) {
  var me = roundButton( 'black white-text'
                      , 'cake'
                      , 'Analyze'
                      , handler
                      )

  var content = $('<div/>').addClass('card-content')

  var closeBtn = $('<a>').addClass('btn').text('close')
                 .click(function() { content.empty(); dom.hide() })

  var dom = $('<div/>').addClass('card')
                       .append(content)
                       .append($('<div/>')
                               .addClass('card-action')
                               .append(closeBtn))
                       .hide()


  function handler () {
    jQuery.post('/analyze', { id: v.ref }, function(res) {
      content.empty().append([aResult(res)])
      dom.show()
    })
    .fail(disconnected)
  }

  return { icon: me, dom: dom }
}


function drawCollapsed(dbgState, lab, v) {
        return drawCollapsedEx(dbgState, lab, v, dbgState.valueOpts());
}


function drawCollapsedEx(dbgState, lab, v, opts) {

  var icons = []    // These go in the pop-up menu
  var here  = []    // This is what shows up when we expand the thing

  // Expandable icon
  switch(v.tag) {
    case 'table':
    case 'user_data':
    case 'closure':
      var expand = drawExpandCollapseIcon(dbgState, v, opts)
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
  if (opts.watchMode !== 'no-watch')
    icons.push(drawAddWatchIcon(dbgState,v,opts))

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
    case 'number':
      icons.push(drawAltRepIcon(v, lab, v.alt))
      break;
  }

  icons.push(drawSetValueIcon(dbgState,lab,v,opts))



  var small = makeMenu(lab,icons)

  return { small: small, big: here }
}



// Additional information about a value
function renderExpanded(dbgState, id, v) {

  switch (v.tag) {
    case 'table': return drawTable(dbgState,id,v)
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
         .addClass('literal tooltipped')
         .attr('data-tooltip', x.tag)
         .text(x.text)
         .tooltip()
}



// Expanded Lua closure
function drawLuaClosure(dbgState, t) {
  var uvs = drawValueList(dbgState,'ups',t.upvalues)
  return $('<div/>').append(uvs)
}




// Expanded user data
function drawUserData(dbgState,t) {
  var d     = $('<table/>')
  var row   = $('<tr/>')
  var right = $('<td/>')

  var bigR  = $('<tr/>')
  var bigC  = $('<td/>')
  bigR.append(bigC)


  if (t.ref !== undefined) {
    var meta = drawCollapsed(dbgState
                            ,$('<div/>')
                            .text('meta ' + t.text)
                            .addClass('code-line galua-small-label')
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
function drawTable(dbgState,id,t) {

  var chunkNum     = 0

  var d = $('<table/>').addClass('striped')

  function addEntries(ents) {

    var thisClass = 'galua-table-chunk-' + chunkNum
    var nextChunk = ++chunkNum

    if (ents.values.length === 0) {
      d.append($('<tr/>')
               .append($('<td/>')
                       .attr('colspan','2')
                       .addClass('galua_remark')
                       .text('empty table')))
    } else
      jQuery.each(ents.values, function(ix,entry) {
        var r = $('<tr/>').addClass(thisClass)
        var k = $('<td/>').append(drawValue(dbgState,entry.key))
        var v = $('<td/>').append(drawValue(dbgState,entry.value))
        r.append([k,v])
        d.append(r)
      })

    var subOpen = false
    var downloaded = false

    if (ents.missing > 0) {
      var r = $('<tr/>')
      var c = $('<td/>')
              .attr('colspan','2')
              .css('cursor','pointer')
              .addClass('galua_remark')
              .text('... ' + ents.missing + ' more ...')
      d.append(r.append(c))
      c.click(function() {
        if (subOpen) {
          $('.' + 'galua-table-chunk-' + nextChunk).hide()
          subOpen = false
        } else {
          if (downloaded) {
            $('.' + 'galua-table-chunk-' + nextChunk).show()
            subOpen = true
          } else
            jQuery.post('/expandTable'
                       , { id: id
                         , from: ents.startIx + ents.values.length
                         }
                       , function(newEnts) {
                           addEntries(newEnts)
                           downloaded = true
                           subOpen = true
                         })
        }
        return false
      })
    }
  }


  addEntries(t)

  if (t.ref !== undefined) {
    var r = $('<tr/>')
    var meta = drawCollapsed(dbgState
                            ,$('<span/>')
                            .text('meta ' + t.text)
                            .addClass('code-line galua-small-label')
                            , t
                            )
    r.append($('<td/>').attr('colspan','2').append(meta.small))

    var r2 = $('<tr/>')
    r2.append($('<td/>').attr('colspan','2').append(meta.big))

    d.append([r,r2])
  }

  return d

}

