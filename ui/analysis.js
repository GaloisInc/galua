
function aIsBottom(v) {
  return v.simple.length === 0 &&
         v.function.length === 0 &&
         v.table.length === 0 &&
         v.reference.length === 0
}

function aValue(glob, v) {
  var opts = []

  function more(name,draw,icon,xs) {
    if (xs.length === 0) return
    var t = $('<div/>').text(name + ' ')

    var cur = null

    jQuery.each(xs, function(ix,r) {
      var btn = $('<i/>')
                .addClass('uk-icon-' + icon)
                .css('cursor','pointer')
                .click(function() {
                    if (cur !== null) {
                      cur.dom.remove()
                      if (cur.ix === ix) { cur = null; return }
                    }
                    var sub = draw(glob,r)
                    cur = { dom: sub, ix: ix }
                    t.append(cur.dom)
                 })
      t.append(btn)
    })

    opts.push(t)
  }

  jQuery.each(v.simple,function(ix,name) {
    var it = $('<span/>').text(name)
    opts.push(it)
  })

  more('table',     aTableV, 'table',  v.table)
  more('function',  aFunV,   'laptop', v.function)
  more('reference', aRefV,  'share',  v.reference)


  var me = $('<div/>')
           .css('display','inline-block')

  jQuery.each(opts, function(ix,opt) {
    if (ix !== 0) me.append('&nbsp;','|','&nbsp;')
    me.append(opt)
  })

  return me
}

function aTableV(glob,v) {
  var t = glob.tables[v]
  var me = $('<table>')
  function row(a,b) {
    me.append($('<tr>')
              .append([ $('<td/>').append(a), $('<td/>').append(b) ]))
  }

  if (!aIsBottom(t.key)) row(aValue(glob,t.key), aValue(glob,t.value))

  jQuery.each(t.attrs, function(key,val) {
    row($('<span/>').text(key),aValue(glob,val))
  })

  row($('<i/>').text('meta'), aValue(glob,t.meta))

  return me
}

function aFunV(glob,v) {
  var t = glob.functions[v]
  var me = $('<div/>').text(JSON.stringify(t,null,2))
  return me
}

function aRefV(glob,v) {
  return aValue(glob, glob.heap[v])
}


function aList(glob,vs) {
  var me = $('<div/>')
           .css('display', 'inline-block')
  jQuery.each(vs.elements, function(ix,v) {
    if (ix !== 0) me.append(', ')
    me.append(aValue(glob,v))
  })
  me.append(', .. ')
  me.append(aValue(glob, vs.default))
  me.append(' .. ')
  return me
}

function aResult(r) {
  var me = $('<div/>')
  if (!aIsBottom(r.raises)) {
    me.append('raises: ', aValue(r.post,r.raises))
      .append($('<br>'))
  }
  if (r.returns !== null) {
    me.append('returns: ', aList(r.post,r.returns))
  }

  return me
}




