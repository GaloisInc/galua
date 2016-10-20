(function( $ ) {

  var areaProps = 'area-props'

  jQuery.fn.areaUI = function(action,a,b) {

    switch(action) {

      case 'disappear':
        if (this.data(areaProps) !== undefined) disappear(this,a)
        return this

      case 'appear':
        if (this.data(areaProps) !== undefined) appear(this,a)
        return this

      case 'update':
        if (this.data(areaProps) !== undefined) update(this,a,b)
        return this

      case 'switcher':
        var me = this
        me.click(function() {
          var cur = $(a).filter('area-ui-selected')
          $(a).areaUI('disappear', function() {
                cur.removeClass('area-ui-selected')
                var newCur = $(b)
                newCur.areaUI('appear', function() {
                  newCur.addClass('area-ui-selected')
                })
          })
        })
        return this
    }
  }

  AreaUI =
    { create: function (opts) {
        var area = $('<div/>')
                   .css('position','absolute')
                   .css('margin','0')
        settings(area,opts)
        return area
      }

    , roundButton: function() {
        var btn = $('<div/>')
                  .css('width', '60px')
                  .css('height','60px')
                  .css('cursor','pointer')
                  .css('border-radius','30px')
                  .css('display','inline-block')
                  .css('margin','2px')
        btn.hover(function() { btn.addClass('area-ui-hover-button') }
                 ,function() { btn.removeClass('area-ui-hover-button') }
                 )

        return btn
      }

    , switcher: function() {
      }


    }

  function dflt(x,y) { return (x === undefined) ? y : x }

  function settings(area,opts) {
    if (opts === undefined) opts = {}
    function get(x,y) { return dflt(opts[x],y) }
    var props = { left:       get('left',0)
                , top:        get('top',0)
                , width:      get('width',64)
                , height:     get('height',64)
                , opacity:    get('opacity',1)

                // For disappearance
                , duration:   get('duration','fast')
                , easing:     get('easing','swing')
                , transition: get('transition','fade')
                }
    area.data(areaProps,props)
        .css('left',    props.left)
        .css('top',     props.top)
        .css('width',   props.width)
        .css('height',  props.height)
        .css('opacity', props.opacity)
  }

  function animate(area,opts) {
    var o = {}
    function get(x) { if (opts[x] === undefined) return; o[x] = opts[x] }

    get('left')
    get('top')
    get('width')
    get('height')
    get('opacity')
    area.animate( o
                , dflt(opts.duration,  'fast')
                , dflt(opts.easing,    'swing')
                , dflt(opts.complete,  function() {})
                )
  }

  function update(area,newOpts,complete) {
    var props = area.data(areaProps)

    function lkp(x) { if (newOpts[x] === undefined) newOpts[x] = props[x] }
    lkp('width')
    lkp('height')
    lkp('left')
    lkp('top')
    lkp('opacity')
    lkp('duration')
    lkp('easing')
    lkp('transition')

    var opts = { complete: function() { settings(area,newOpts)
                                        if (complete !== undefined) complete()
                                      }
               }
    animate(area,jQuery.extend(opts,newOpts))
  }

  function disappear(area, complete) {
    var props = area.data(areaProps)
    var opts  = { duration: props.duration
                , easing:   props.easing
                , complete: function() { area.hide()
                                         if (complete !== undefined) complete()
                                       }
                }
    switch(props.transition) {
      case 'fade':    opts.opacity  = 0; break
      case 'left':    opts.width    = 0; break
      case 'top':     opts.height   = 0; break
      case 'right':   opts.width    = 0
                      opts.left     = props.width + props.left
                      break
      case 'bottom':  opts.height   = 0
                      opts.top      = props.height + props.top
                      break
      case 'hmiddle': opts.width    = 0
                      opts.left     = props.width/2 + props.left
                      break
      case 'vmiddle': opts.height   = 0
                      opts.top      = props.height/2 + props.top
                      break
      case 'middle':  opts.width    = 0
                      opts.height   = 0
                      opts.left     = props.width/2 + props.left
                      opts.top      = props.height/2 + props.top
                      break
    }
    animate(area,opts)
  }


  function appear(area, complete) {
    var opts = { complete: complete }
    var props = area.data(areaProps)
    area.show()
    animate(area,jQuery.extend(opts,props))
  }






})(jQuery)

