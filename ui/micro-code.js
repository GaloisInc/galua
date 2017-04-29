
function drawMicroCode(dbg) {
  var fid = dbg.programCounter.fid

  jQuery.post('/micro-function' , { fid: fid }, function(cfg) {
    console.log(cfg)

    var els = []
    jQuery.each(cfg,function(l,b) {
      var lab = b.stmts.join('\n')
      els.push({ data: { id: l, label: lab }})

      jQuery.each(b.next, function(ix,to) {
        els.push({ data: { source: l, target: to } })
      })

    })

    var cont = $('#value-container').empty()
               .css('width','1000px').css('height','800px')

    $('#value-pane').openModal()


    var cy = cytoscape
    ({ container: cont
     , elements: els
     , style: [ { selector: 'node'
                , style: { label: 'data(label)'
                         , 'text-wrap': 'wrap'
                         , 'text-halign': 'right'
                         }
                }
              ]

    , layout: { name: 'dagre' }
    })



    })
}
