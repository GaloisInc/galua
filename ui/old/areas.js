function module_Areas () {

  var scale = 12
  var methodField = 'area-methods'

  // XXX
  $('html').css('height','100%').css('padding','0').css('margin','0')
  $('body').css('height','100%').css('padding','0').css('margin','0')

  function dflt(x,y) { return (x === undefined) ? y : x }
  function rel(x) { return (100/scale*x) + '%' }

  jQuery.fn.area = function() {
    return this.each(function () {
      this.
    }
  }

  function create(opts) {

    var props

    function get(x,y) { return dflt(opts[x],y) }
    var props
    var area = $('<div/>')
               .css('position','absolute')
               .css('margin','0')

    settings(opts)

    area.data(methodField, { appear:    appear
                           , disappear: disappear
                           , update:    update
                           })

    return area

    function settings(opts) {
      if (opts === undefined) opts = {}
      function get(x,y) { return dflt(opts[x],y) }
      props = { left:       get('left',0)
              , top:        get('top',0)
              , width:      get('width',1)
              , height:     get('height',1)
              , opacity:    get('opacity',1)

              // For disappearance
              , duration:   get('duration','fast')
              , easing:     get('easing','swing')
              , transition: get('transition','fade')
              }
      area.css('left',    rel(props.left))
          .css('top',     rel(props.top))
          .css('width',   rel(props.width))
          .css('height',  rel(props.height))
          .css('opacity', props.opacity)
    }

   function animate(opts) {
      var o = {}
      get('left')
      get('top')
      get('width')
      get('height')
      if (opts.opacity !== undefined) o.opacity = opts.opacity
      area.animate( o
                  , dflt(opts.duration,  'fast')
                  , dflt(opts.easing,    'swing')
                  , dflt(opts.complete,  function() {})
                  )

      function get(x) {
        if (opts[x] === undefined) return
        o[x] = rel(opts[x])
      }
    }

    function update(newOpts,complete) {
      function lkp(x) { if (newOpts[x] === undefined) newOpts[x] = props[x] }
      lkp('width')
      lkp('height')
      lkp('left')
      lkp('top')
      lkp('opacity')
      lkp('duration')
      lkp('easing')
      lkp('transition')

      var opts = { complete: function() { settings(newOpts)
                                          if (complete !== undefined) complete()
                                        }
                 }
      animate(jQuery.extend(opts,newOpts))
    }

    function disappear(complete) {
      var opts = { duration: props.duration
                 , easing:   props.easing
                 , complete: function() { area.hide()
                                          if (complete !== undefined)
                                            complete()
                                         }
                 }
      switch(props.transition) {
        case 'fade':    opts.opacity = 0; break
        case 'left':    opts.width   = 0; break
        case 'top':     opts.height  = 0; break
        case 'right':   opts.width   = 0
                        opts.left    = props.width + props.left
                        break
        case 'bottom':  opts.height  = 0
                        opts.top     = props.height + props.top
                        break
        case 'hmiddle': opts.width  = 0
                        opts.left = props.width/2 + props.left
                        break
        case 'vmiddle': opts.height = 0
                        opts.top    = props.height/2 + props.top
                        break
        case 'middle':  opts.width  = 0
                        opts.height = 0
                        opts.left   = props.width/2 + props.left
                        opts.top    = props.height/2 + props.top
                        break
      }

      animate(opts)
    }

    function appear(complete) {
      var opts = { complete: complete }
      area.show()
      animate(jQuery.extend(opts,props))
    }


    function appearThis(thing,complete) {
      thing.data(methodField).appear(complete)
    }


    function disappearThis(thing,complete) {
      thing.data(methodField).disappear(complete)
    }
  }

  function switcher(swi,showThese,hideThese,complete) {
    var sh = showThese
    var hi = hideThese

    var count = 0
    function done() {
      if (count <= 0) return
      --count
      if (count === 0) {
        if (complete !== undefined) complete()
        var tmp = sh
        sh = hi
        hi = tmp
     }
    }

    swi.click(function() {
      if (count > 0) {
        jQuery.each(sh,function(ix,thing) { $(thing).finish() })
        jQuery.each(hi,function(ix,thing) { $(thing).finish() })
      }
      count = sh.length + hi.length
      jQuery.each(sh,function(ix,thing) { appearThis(thing,done) })
      jQuery.each(hi,function(ix,thing) { disappearThis(thing,done) })
      return false
    })
  }

  return { create: create
         , switcher: switcher
         }


}

