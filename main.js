
var dims = {x:1600, y:800}
var elem = document.getElementById('canvas-div');
var nElems  = 1000
var nMoving = 350


if(0) {
    //------------------------------
    var two = new Two({
      type: Two.Types.canvas,
      width: dims.x,
      height: dims.y
    }).appendTo(elem);

    var group = two.makeGroup();
    var circles = [...Array(nElems).keys()].map(function(i) {
      var c = two.makeCircle(Math.floor(Math.random() * dims.x), Math.floor(Math.random() * dims.y), 15);
      // c.fill = ['red', 'green', 'blue'][Math.floor(Math.random() * 3)]
      c.fill = 'grey'
      c.noStroke()
      group.add(c)
      return c
    })
    circles.reverse() // Side-effects!
    console.log("reversed")
    two.update();
    // Bind a function to scale and rotate the group
    // to the animation loop.
    two.bind('update', function(frameCount) {
       if ((frameCount % 1) !== 0) return;
       for (var i = 0; i < Math.min(circles.length, nMoving); ++i) {
         var c = circles[i]
         c.fill = 'red'
         c.translation.y = (c.translation.y + 1) % dims.x
       }
    }).play(); // Finally, start the animation loop
    //------------------------------

} else {
    // State
    var canvas = document.getElementById('canvas');
    var c = canvas.getContext('2d');
    // console.log(c)

    function drawCircle(c,x,y,r) {
      c.beginPath();
      c.arc(x,y,r, 0, 2 * Math.PI, false);
      c.fill();
    }

    const STYLE_TYPES  = {fillColor:1|0}
    const DRAWING_TYPES  = {circle:1|0, transf: 2|0, style: 3|0, ap: 4|0}

    function renderStyle1(name, value, context) {
      switch (name) {
        case STYLE_TYPES.fillColor:
          context.fillStyle = value; // TODO opacity etc
          break;
        // default:
          // throw ('renderStyle: Unknown style ' + name);
      }
    }
    /*
    Render the given drawing object using the given context.
    Does NOT clear the rectangle beforehand.
    FIXME adjust co-ordinates based on opts.originPlacement, opts.dimensions (a la Drawing)
    */
    function renderTransf(opts,context,drawing) {
                context.save()
                let [a,b,c,d,e,f] = drawing.transf;
                context.transform(a,b,c,d,e,f)
                renderDrawing(opts, drawing.sub, context)
                context.restore()
    }
    function renderStyle(opts,context,drawing) {
                context.save()
                renderStyle1(drawing.name, drawing.value, context)
                renderDrawing(opts, drawing.sub, context)
                context.restore()
    }
    function renderDrawing(opts, drawing, context) {
      // if (opts === undefined) opts = {};

      if (drawing.type === DRAWING_TYPES.circle) {
        drawCircle(context, drawing.x, drawing.y, drawing.rad);
      } else
      if (drawing.type === DRAWING_TYPES.transf) {
        renderTransf(opts,context,drawing)
      } else
      if (drawing.type === DRAWING_TYPES.style) {
        renderStyle(opts,context,drawing)
      } else
      if (drawing.type === DRAWING_TYPES.ap) {
          for (var i = 0, len = drawing.children.length; i < len; i++) {
            renderDrawing(opts, drawing.children[i], context);
          }
      } else {
        // throw ('renderDrawing: Unknown drawing type ' + drawing.type);
      }
    }

    function replicate(n,x) {
      return [...Array(n).keys()].map(ignored => x)
    }
    function enumFromZeroTo(n) {
      return [...Array(n).keys()]
    }

    // const drawingDef = {type:DRAWING_TYPES.transf, transf:[2,0,0,1,0,0], sub:
    //   {type:DRAWING_TYPES.style, name:'fillColor', value:'grey', sub:
    //     {type:DRAWING_TYPES.ap, children:
    //       replicate(nElems, {type: DRAWING_TYPES.circle, x:10, y:10, rad:10}
    //       )
    //       }
    //   }
    // };

    // const drawingDef = {type:DRAWING_TYPES.transf, transf:[1,0,0,1,40,40], sub:
    //   {type:DRAWING_TYPES.style, name:'fillColor', value:'grey', sub:
    //     {type:DRAWING_TYPES.ap, children:
    //       replicate(nElems, {type: DRAWING_TYPES.circle, x:0, y:0, rad:10}
    //       )
    //       }
    //   }
    // };
    const nonMovingDef =
      {type:DRAWING_TYPES.style, name:STYLE_TYPES.fillColor, value:'blue', sub:
        {type:DRAWING_TYPES.ap, children:
          enumFromZeroTo(nElems-nMoving).map(function(_) {return {type: DRAWING_TYPES.circle, x:Math.floor(Math.random()*dims.x), y:Math.floor(Math.random()*dims.y), rad:15} } )
        }
      }
    ;
    var movingDef_positions = []; // [[x1,y1],[x2,y2]...]
    function movingDef() {
      var circles = []
      for(var i = 0; i < nMoving; ++i) {
        let [x, y] = (movingDef_positions[i] || [ Math.random()*dims.x, Math.random()*dims.y ]);
        // Update position state (for next time)
        movingDef_positions[i] = [(x + 1) % dims.x, (y + 1) % dims.y];
        circles.push({type: DRAWING_TYPES.circle, x:x, y:y, rad:2})
      }
      return {type:DRAWING_TYPES.style, name:STYLE_TYPES.fillColor, value:'red', sub: {type:DRAWING_TYPES.ap, children: circles}}
    }
    function drawingDef() {
      let md = movingDef()
      return {type:DRAWING_TYPES.ap, children: [nonMovingDef, md]}
    }


    function setup () {
      // c.fillCircle(50,50,50,50)
      // drawCircle(c, dims.x/2, dims.x/2, 5)

      c.translate(0.5,0.5);
    }
    function drawFrame() {
      c.clearRect(0, 0, dims.x, dims.y);
      renderDrawing({},
        drawingDef()
        , c);
      // c.save()
      // c.fillStyle = 'green';
      // for(var i = 0; i<nElems; ++i) {
      //   drawCircle(c, dims.x * Math.random(), dims.x * Math.random(), 5)
      // }
      // c.restore()
    }

    function loop() {
      drawFrame()
      requestAnimationFrame(loop)
    }
    setup()
    loop()


}












//
//
// type CanvasName = String
// renderFirst :: CanvasName -> Drawing -> IO ()
// renderSubsequent :: CanvasName -> Drawing -> IO ()
//
//
// type FrameRate = Double
// animate :: FrameRate -> Behavior (Drawing a) -> IO (IO ())
//
//
//
//
// // Render opts
// {dimensions:[200,400], originPlacement:"center",...}
//
// // Drawing
// // NOTE all co-ordinates here uses Drawing/Math conventions, convert to Canvas conventions by pushing a matrix before starting to render
// // NOTE the rendering phase will be a simple traversal of this tree
//
// "circle"
// "rect"
// "line"
// ["lines",true,0.5,0,3.2,-2.1}
// ["text","Hello!"]
// ["mask",d1,d2]
// ["transf",[1,0,0,1,0,0], d1]
// ["style",{N:V,...}, d1]
  // NOTE whenever we travel down one of these, we will use save()/restore()
  // If the styles we render are not handled by save/restore, we have to use a separate stack (hope not!)
//   "fillColor"
//   "fillColorA"
//   "strokeColor"
//   "strokeColorA"
//   "strokeWidth"
//   "dashing"
//   // TODO text, gradients
// ["ap",d1...]
// []


// TODO ignore hanlers, SVG embed, CircleSector, RectRounded, text, mask, special styles/gradients
// Provide binary and list monoid (for faster mconcat)
