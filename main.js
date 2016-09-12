
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







/*
Alternative crazy idea:
  Instead of letting core functions allocate JS obejcts we will
    - Let core function call into asm.js functions that returns "pointers" (i.e. numbers indexing objects in their local heap)
    - At render time, pass such as pointer (representing a drawing tree) to an asm function
    - Expose functions to "free" these pointers/drawings (need to hook into HS garbage collector with Foreign.ForeignPtr
 for this to work)

 How to generate heap HEAP8 = new Int8Array(buffer);
   HEAP16 = new Int16Array(buffer);
    [i>>1]
   HEAP32 = new Int32Array(buffer);
    [i>>2]
   HEAPU8 = new Uint8Array(buffer);
    [i>>0]

   HEAPU16 = new Uint16Array(buffer);
    [i>>1]
   HEAPU32 = new Uint32Array(buffer);
    [i>>2]

   HEAPF32 = new Float32Array(buffer);
    [i>>2]
   HEAPF64 = new Float64Array(buffer);
    [i>>3]



 Validator
  http://anvaka.github.io/asmalidator/

 Each "drawing" is a tuople stored on the heap a tuple of 32 bytes (i.e.8 fields of I32, U32, F32)
 First fields is a type id, the rest is parameters depending on type

    Name        ID(I32) Params
    (offset:)   0       1       2       3       4       5       6      7
    circle      0       x:f32   y:f32   rad:f32
    transf      32      a:f32   b:f32   c:f32   d:f32   e:f32   f:f32 dr:i32
    fillColor   64      r       g       b       a
    ap2         128     dr1:i32 dr2:i32


*/

function AsmDrawingRenderer(stdlib, foreign, heap) {
  "use asm";

  // var HEAP16 = new stdlib.Int16Array(heap);
  var HEAP32 = new stdlib.Int32Array(heap);
  // var HEAPU8 = new stdlib.Uint8Array(heap);
  // var HEAPU16 = new stdlib.Uint16Array(heap);
  // var HEAPU32 = new stdlib.Uint32Array(heap);
  var HEAPF32 = new stdlib.Float32Array(heap);
  // var HEAPF64 = new stdlib.Float64Array(heap);

  var _debug = foreign.debug;
  var _beginPath = foreign.beginPath;
  var _fill = foreign.fill;
  var _fillStyle = foreign.fillStyle;
  var _arc = foreign.arc;
  var _save = foreign.save;
  var _restore = foreign.restore;
  var _transform = foreign.transform;


  var tuplesCreated = 0

  // Return pointer to the first slot as a pointer (byte offset)
  // Add slot count to this, so for slot n in pointer p, use [(p + (n<<2)) >> 2]
  function newTuple() {
    // Treet first 8 bytes in
    var next = 0;
    next = tuplesCreated
    tuplesCreated = tuplesCreated + 1|0
    // return (next * 4)|0
    return ((next * 8) << 2)|0
  }

  // Double ^ 3 -> Drawing*
  function primCircle(x, y, rad) {
    x = +x;
    y = +y;
    rad = +rad;

    var p = 0

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = 0|0
    HEAPF32[(p + (1<<2)) >> 2] = x
    HEAPF32[(p + (2<<2)) >> 2] = y
    HEAPF32[(p + (3<<2)) >> 2] = rad
    return p|0
  }
  // Double ^ 4 -> Drawing*
  function primFillColor(r,g,b,a,dr) {
    r = +r;
    g = +g;
    b = +b;
    a = +a;
    dr = dr|0;

    var p = 0

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = 64|0
    HEAPF32[(p + (1<<2)) >> 2] = r
    HEAPF32[(p + (2<<2)) >> 2] = g
    HEAPF32[(p + (3<<2)) >> 2] = b
    HEAPF32[(p + (4<<2)) >> 2] = a
    HEAP32 [(p + (5<<2)) >> 2] = dr
    return p|0
  }
  // Double ^ 6 -> Drawing*
  function primTransf(a,b,c,d,e,f,dr) {
    a = +a;
    b = +b;
    c = +c;
    d = +d;
    e = +e;
    f = +f;
    dr = dr|0;

    var p = 0

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = 32|0
    HEAPF32[(p + (1<<2)) >> 2] = a
    HEAPF32[(p + (2<<2)) >> 2] = b
    HEAPF32[(p + (3<<2)) >> 2] = c
    HEAPF32[(p + (4<<2)) >> 2] = d
    HEAPF32[(p + (5<<2)) >> 2] = e
    HEAPF32[(p + (6<<2)) >> 2] = f
    HEAP32 [(p + (7<<2)) >> 2] = dr
    return p|0
  }
  // Drawing* -> Drawing* -> Drawing*
  function primAp2(dr1,dr2) {
    dr1 = dr1|0;
    dr2 = dr2|0;

    var p = 0

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = 128|0
    HEAP32 [(p + (1<<2)) >> 2] = dr1
    HEAP32 [(p + (2<<2)) >> 2] = dr2
    return p|0
  }
  // Drawing* -> Drawing* -> Drawing*
  // function primAp3(dr1,dr2) {
  //   dr1 = dr1|0;
  //   dr2 = dr2|0;
  // }


  function drawCircle(x,y,r) {
    x=+x
    y=+y
    r=+r
    var bp = 0;

    _beginPath();
    _arc(x,y,r,0, 6.283185307179586,0/*false*/);
    _fill();
  }

  // function renderFillColor(r, g, b, a) {
  //   _fillStyle_(r, g, b, a)
  // }
  // function renderTransf(opts,a,b,c,d,e,f,sub) {
  //     _save()
  //     _transform(a,b,c,d,e,f)
  //     render(opts, sub)
  //     _restore()
  // }


  // Opts* -> Drawing* -> ()
  function render(opts,dr) {
    opts = opts|0;
    dr = dr|0;
    // var drType = 0|0;

    // var t = 0
    // t = (newTuple()|0)
    // _debug(t|0)
    // t = (newTuple()|0)
    // _debug(t|0)
    // t = (newTuple()|0)
    // _debug(t|0)
    //
    // drawCircle(1.,2.,3.)

    var drType = 0
    var x = 0.
    var y = 0.
    var r = 0.
    var dr1 = 0
    var dr2 = 0

    //DEBUG see below
    var i = 0

    drType = HEAP32[(dr+(0<<2)) >> 2]|0;
    switch (drType|0) {
      case 0:
          x = +HEAPF32[(dr+(1<<2)) >> 2];
          y = +HEAPF32[(dr+(2<<2)) >> 2];
          r = +HEAPF32[(dr+(3<<2)) >> 2];
          drawCircle(x,y,r)
        break;
      case 64:
        break;
      case 32:
        break;
      case 128:
          dr1 = HEAP32[(dr+(1<<2)) >> 2]|0;
          dr2 = HEAP32[(dr+(2<<2)) >> 2]|0;

          render(opts,dr1)
          render(opts,dr2)


        break;
    }

    // if (drType == 0) {
    //   // drawCircle(context, drawing.x, drawing.y, drawing.rad);
    // } else
    // if (drType == 32) {
    // //   // renderTransf(opts,context,drawing)
    // } else
    // if (drType == 64) {
    // //   // renderStyle(opts,context,drawing)
    // } else
    // if (drType == 128) {
    // //   // for (var i = 0, len = drawing.children.length; i < len; i++) {
    // //     // render(opts, drawing.children[i]);
    //   }
    // }
  }
  // etc
  return { render : render
      , primCircle : primCircle
      , primFillColor : primFillColor
      , primTransf : primTransf
      , primAp2 : primAp2
    }
}

function createRenderer() {
  // TODO generate and link a proper drawing context

  // Renderer is linked here...
  return new AsmDrawingRenderer(window,
      { beginPath:
        x=>null
        // x=>console.log('beginPath')
      , fill:
        // x=>console.log('fill')
        x=>null
      , fillStyle:
        // x=>console.log('fillStyle_')
        x=>null
      , arc:
        // x=>console.log('arc')
        x=>null
      , save:
      // x=>console.log('x')
        x=>null
      , restore:
      // x=>console.log('x')
        x=>null
      , transform:
      // x=>console.log('x')
        x=>null
      , debug:
        x=>console.log(x)
      }, new ArrayBuffer( 0x10000))
}

function test () {
  r = createRenderer()

  r.render(0,
     r.primAp2(r.primCircle(), r.primCircle())
   )
}

// test()






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
