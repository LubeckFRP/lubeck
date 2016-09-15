
var dims = {x:1900, y:1500}
var nElems  = 1000
var nMaxFrames = 60*1
var nHeapSize = 0x1000000 // 2^24 == 16,777,216 B


/*
  A renderer wraps a Canvas API drawing context which may render to an on-screen canvas or an
  off-screen buffer.

  Each renderer renders drawings, represented as a tree stored on the renderer's heap.

  Create a renderer using createRenderer(canvasApiContext).

  Each renderer exposes commands to create new drawings and a function render(options,drawing),
  which immediatly renders the given drawing to the underlying drawing context.

  You can have multiple renderers, but drawings can not be shared between renderers.


  IMPLEMENTATION NOTES

  NOTE Memory management

  TBD

  NOTE Heap layout

  The renderer heap is broken into regions, delimited by the HEAP_N constants below.

  These values are all asmjs style pointers and represent an offset into the heap
  buffer, in bytes. The main part of the heap is used to store "tuples", which are simply
  32-byte regions stored at values > HEAP_TUPLES_OFFSET.

  By convention, each tuple represent 8 values of 4 bytes each. They are used to store one
  of the following:
    - Pointers to other tuples
    - 32 bit signed ints
    - 32 bit floats

  We refer to these as "slots" indexed [0..7]. The 0-th slot of each tuple is a type tag which
  is one of the NODE_TYPE_N values.

  Each node in the drawing tree is represented as a tuple. For the exact values of
  the slot see the corresponding constructor function (i.e. search for NODE_TYPE_RECT to
  verify that RECT nodes contain for floating point-values, representing, x, y, width and height).

*/

// This buffer is used to return color values to the underlying context (as UTF8 strings).
#define HEAP_COLOR_BUFFER_OFFSET 0
// This region is not currently used
#define HEAP_UNUSED_OFFSET       36
// This region stores the tuples. Its size is (heap size - HEAP_TUPLES_OFFSET).
#define HEAP_TUPLES_OFFSET       0x1000 // 4096


// Primitives
#define NODE_TYPE_CIRCLE          0
#define NODE_TYPE_RECT            1

// Styles
#define NODE_TYPE_FILL_COLOR      64
#define NODE_TYPE_STROKE_COLOR    65
#define NODE_TYPE_LINE_WIDTH      66
#define NODE_TYPE_LINE_CAP        67
#define NODE_TYPE_LINE_JOIN       68
#define NODE_TYPE_LINE_DASH       69
#define NODE_TYPE_GRADIENT_LINEAR 70
// TODO text, embedded bitmaps, composites/masks

// Affine transformations
#define NODE_TYPE_TRANSF          127

// Groups (named for the verb "append")
#define NODE_TYPE_AP2             128
#define NODE_TYPE_AP3             129
#define NODE_TYPE_AP4             130

function AsmDrawingRenderer(stdlib, foreign, heap) {
  "use asm";

  // var HEAP16 = new stdlib.Int16Array(heap);
  var HEAP32 = new stdlib.Int32Array(heap);
  var HEAPU8 = new stdlib.Uint8Array(heap);
  // var HEAPU16 = new stdlib.Uint16Array(heap);
  // var HEAPU32 = new stdlib.Uint32Array(heap);
  var HEAPF32 = new stdlib.Float32Array(heap);
  // var HEAPF64 = new stdlib.Float64Array(heap);

  var _debug = foreign.debug;
  var _beginPath = foreign.beginPath;
  var _fill = foreign.fill;
  var _fillStyleRGBA = foreign.fillStyleRGBA;
  var _fillStyleFromColorBuffer = foreign.fillStyleFromColorBuffer
  var _arc = foreign.arc;
  // var _rect = foreign.rect;
  var _fillRect = foreign.fillRect;
  var _save = foreign.save;
  var _restore = foreign.restore;
  var _transform = foreign.transform;
  var _floor = stdlib.Math.floor;
  var _imul = stdlib.Math.imul;
  var _max = stdlib.Math.max;
  var _min = stdlib.Math.min;

  var tuplesCreated = 0

  // Return pointer to the first slot as a pointer (byte offset)
  // Add slot count to this, so for slot n in pointer p, use [(p + (n<<2)) >> 2]
  function newTuple() {
    // Treet first 8 bytes in
    var next = 0;
    next = tuplesCreated
    tuplesCreated = tuplesCreated + 1|0
    // return (next * 4)|0
    return ((next * 8) << 2) + HEAP_TUPLES_OFFSET|0
  }

  // Return offset of the string buffer as a pointer (byte offset)
  function getStringBufferOffset() {
    return 0
  }


  function drawCircle(x,y,r) {
    x=+x
    y=+y
    r=+r
    _beginPath();
    _arc(x,y,r,0, 6.283185307179586,0/*false*/);
    _fill();
  }
  function drawRect(x,y,w,h) {
    x=+x
    y=+y
    w=+w
    h=+h
    // _rect(x,y,w,h);
    // _fill();
    _fillRect(x,y,w,h);
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


  // Writes a string such as '123' or '255' to the given pointer based
  // on the given value in [0..1].
  function writeColorString(ptr,n) {
    ptr = ptr|0
    n = +n
    // First multiply float by 256 and floor
    var n2 = 0
    var d1 = 0
    var d2 = 0
    var d3 = 0



    n2 = ~~_floor(
      _max(0.,_min(n,1.))
        * 256.0)|0 // n2 :: signed

    // Then calculate digits (div by 10 and 100)
    d1 = (((n2|0) / (100|0))|0) % (10|0) | 0 // (n2 / 100) % 10
    d2 = (((n2|0) / (10 |0))|0) % (10|0) | 0 // (n2 / 10 ) % 10
    d3 = (((n2|0) / (1  |0))|0) % (10|0) | 0 // (n2 / 1  ) % 10

    // Then convert digits to char codes and write to ptr
    HEAPU8[(ptr + 0) >> 0] = 48 + d1
    HEAPU8[(ptr + 1) >> 0] = 48 + d2
    HEAPU8[(ptr + 2) >> 0] = 48 + d3
  }

  // Writes a string such as '0.25' to the given pointer based
  // on the given value in [0..1].
  function writeAlphaString(ptr,col) {
    ptr = ptr|0
    col = +col

    // Filter out numbers outside [0..1]
    // Then multiply float by 100 and floor
    // Then calculate digits (div by 10 and 100)
    // Then convert digits to char codes and write to ptr (with the dot)
    // FIXME
  }

  // Write the given RGBA as a Canvas API style color string to the string buffer
  // Double ^ 4 -> ()
  function writeRGBAStringToBuffer(r,g,b,a) {
    r=+r
    g=+g
    b=+b
    a=+a

    // Prelude Data.Bits> fmap fromEnum "rgba(255,  0,  0,0.50)"
    // [114,103,98,97,40,50,53,53, 44,32,32,48,44,32,32,48,44,48,46,53,48,41]

   // FIXME write opacity
   // FIXME do not repeatedly write "rgba" etc (just once before 1st render)

    // Zero is the string buffer offset (see above)
    HEAPU8 [(0 + (0<<0)) >> 0] = 114 // 'r'
    HEAPU8 [(0 + (1<<0)) >> 0] = 103 // 'g'
    HEAPU8 [(0 + (2<<0)) >> 0] = 98 // 'b'
    HEAPU8 [(0 + (3<<0)) >> 0] = 97 // 'a'
    HEAPU8 [(0 + (4<<0)) >> 0] = 40 // '('
    writeColorString(5, r)
    HEAPU8 [(0 + (8<<0)) >> 0] = 44 // ','
    writeColorString(9, g)
    HEAPU8 [(0 + (12<<0)) >> 0] = 44 // ','
    writeColorString(13, b)
    HEAPU8 [(0 + (16<<0)) >> 0] = 44 // ','
    HEAPU8 [(0 + (17<<0)) >> 0] = 48 // 'X'
    HEAPU8 [(0 + (18<<0)) >> 0] = 46 // '.'
    HEAPU8 [(0 + (19<<0)) >> 0] = 53 // 'X'
    HEAPU8 [(0 + (20<<0)) >> 0] = 48 // 'X'
    HEAPU8 [(0 + (21<<0)) >> 0] = 41 // ')'

  }

  // Double ^ 3 -> Drawing*
  function primCircle(x, y, rad) {
    x = +x;
    y = +y;
    rad = +rad;

    var p = 0

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_CIRCLE|0
    HEAPF32[(p + (1<<2)) >> 2] = x
    HEAPF32[(p + (2<<2)) >> 2] = y
    HEAPF32[(p + (3<<2)) >> 2] = rad
    return p|0
  }
  // Double ^ 4 -> Drawing*
  function primRect(x, y, w, h) {
    x = +x;
    y = +y;
    w = +w;
    h = +h;

    var p = 0

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_RECT|0
    HEAPF32[(p + (1<<2)) >> 2] = x
    HEAPF32[(p + (2<<2)) >> 2] = y
    HEAPF32[(p + (3<<2)) >> 2] = w
    HEAPF32[(p + (4<<2)) >> 2] = h
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
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_FILL_COLOR|0
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
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_TRANSF|0
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
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_AP2|0
    HEAP32 [(p + (1<<2)) >> 2] = dr1
    HEAP32 [(p + (2<<2)) >> 2] = dr2
    // console.log("Creating ap2 drawing: ", dr1, dr2)
    return p|0
  }
  // Drawing* -> Drawing* -> Drawing*
  // function primAp3(dr1,dr2) {
  //   dr1 = dr1|0;
  //   dr2 = dr2|0;
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
    var w = 0.
    var h = 0.
    var r = 0.
    var a = 0.
    var b = 0.
    var c = 0.
    var d = 0.
    var e = 0.
    var f = 0.
    // var r = 0.
    var g = 0.
    // var b = 0.
    var dr1 = 0
    var dr2 = 0

    //DEBUG see below
    var i = 0
    var cont = 0

    do {
    cont = 0
    drType = HEAP32[(dr+(0<<2)) >> 2]|0;

    switch (drType|0) {

      case NODE_TYPE_CIRCLE:
        x = +HEAPF32[(dr+(1<<2)) >> 2];
        y = +HEAPF32[(dr+(2<<2)) >> 2];
        r = +HEAPF32[(dr+(3<<2)) >> 2];
        drawCircle(x,y,r)
        // console.log("Rendering circle: ", x, y, r)
        break;

      case NODE_TYPE_RECT:
        x = +HEAPF32[(dr+(1<<2)) >> 2];
        y = +HEAPF32[(dr+(2<<2)) >> 2];
        w = +HEAPF32[(dr+(3<<2)) >> 2];
        h = +HEAPF32[(dr+(4<<2)) >> 2];
        drawRect(x,y,w,h)
        // console.log("Rendering circle: ", x, y, r)
        break;

      case NODE_TYPE_FILL_COLOR:
        r = +HEAPF32[(dr+(1<<2)) >> 2];
        g = +HEAPF32[(dr+(2<<2)) >> 2];
        b = +HEAPF32[(dr+(3<<2)) >> 2];
        a = +HEAPF32[(dr+(4<<2)) >> 2];
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        // console.log("Rendering fill: ", r, g, b, a)
        // FIXME selective version of save/restore
        _save()

        // _fillStyleRGBA(r,g,b,a)
        writeRGBAStringToBuffer(r,g,b,a)
        _fillStyleFromColorBuffer()

        render(opts,dr1)
        _restore()
        break;


      case NODE_TYPE_TRANSF:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        e = +HEAPF32[(dr+(5<<2)) >> 2];
        f = +HEAPF32[(dr+(6<<2)) >> 2];
        dr1 = HEAP32[(dr+(7<<2)) >> 2]|0;
        // TODO render transf

        // FIXME selective version of save/restore
        // Or simply apply inverted matrix when done http://stackoverflow.com/a/18504573
        _save()
        _transform(a,b,c,d,e,f)
        render(opts,dr1)
        _restore()
        break;

      case NODE_TYPE_AP2:
        dr1 = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr2 = HEAP32[(dr+(2<<2)) >> 2]|0;

        render(opts,dr1)

        render(opts,dr2)
        // Manual tail-call opt: Instead of calling 'render(opts,dr2)', we update the parameters and set 'cont = 1'
        // opts = opts
        // dr = dr2
        // cont = 1

        break;
    }
    }
    while(cont);
  }

  // etc
  return { render : render
      , primCircle : primCircle
      , primRect : primRect
      , primFillColor : primFillColor
      , primTransf : primTransf
      , primAp2 : primAp2
    }
}

var globalHepRef = {}

function createRenderer(c2) {
  // TODO generate and link a proper drawing context
  const c = c2
  // Renderer is linked here...

  var heap = new ArrayBuffer(nHeapSize)
  globalHepRef = heap // TODO debug
  var HEAPU8 = new Uint8Array(heap);
  var colorBuffer = new Uint8Array(heap, HEAP_COLOR_BUFFER_OFFSET, 22);
  var utf8d = new TextDecoder("utf-8");

  var res = new AsmDrawingRenderer(window,
      { beginPath:
        function (x) { c.beginPath() }
        // x=>console.log('beginPath')
      , fill:
        // x=>console.log('fill')
        function (x) { c.fill() }
      , fillStyleFromColorBuffer:
        function () {
          c.fillStyle = utf8d.decode(colorBuffer)
        }
      , fillStyleRGBA:
        // x=>console.log('fillStyle_')
        // FIXME
        // (r,g,b,a)=>console.log(r,g,b,a)
        function (r,g,b,a) {

          // c.fillStyle = "red"
          //
          // c.fillStyle = "rgb(0,255,0)"
          //
          // c.fillStyle = "rgba(0,0,255,0.20)"

          c.fillStyle = "".concat(
              "rgba("
            , Math.floor(256*r)
            , ","
            , Math.floor(256*g)
            , ","
            , Math.floor(256*b)
            , ","
            , +a
            , ")")
        }
      , arc:
        // x=>console.log('arc')
        // TODO is bind() faster than this closure wrapping?
        function (x,y,r) {
          x = +x
          y = +y
          r = +r
          c.arc(x,y,r, 0, 6.283185307179586, false)
        }
      // , rect:
      // // x=>console.log('x')
      //   function (x,y,w,h) {
      //     x = +x
      //     y = +y
      //     w = +w
      //     h = +h
      //     c.rect(x,y,w,h)
      //   }
      , fillRect:
      // x=>console.log('x')
        function (x,y,w,h) {
          x = +x
          y = +y
          w = +w
          h = +h
          c.fillRect(x,y,w,h)
        }
      , save:
      // x=>console.log('x')
        function (x) { c.save() }
      , restore:
      // x=>console.log('x')
        function (x) { c.restore() }
      , transform:
      // x=>console.log('x')
        function (a,b,c_,d,e,f) {
          a = +a
          b = +b
          c_ = +c_
          d = +d
          e = +e
          f = +f

          c.transform(a,b,c_,d,e,f)
          // c.scale(a,d)
          // c.translate(e,f)
        }
      , debug:
        function (x) { console.log(x) }
      }, heap) // FIXME trim

  // Some helpers

  res.ap = function(xs) {
    // TODO better to balance this tree?
    // Or use n-ary nodes where n > 2 (?)
    var empty = r.primCircle(0,0,0) // TODO proper empty drawing
    var res = xs.reduce(function (a,b) {
      return r.primAp2(b,a)
    }, empty)
    return res
  }
  res.scale = function (a,dr) {
    return res.primTransf(a,0,0,a,0,0,dr)
  }
  res.scaleXY = function (x,y,dr) {
    return res.primTransf(x,0,0,y,0,0,dr)
  }
  res.translate = function (a,b,dr) {
    return res.primTransf(1,0,0,1,a,b,dr)
  }
  res.translateX = function (a,dr) {
    return res.primTransf(1,0,0,1,a,0,dr)
  }
  res.translateY = function (b,dr) {
    return res.primTransf(1,0,0,1,0,b,dr)
  }
  res.red = function (dr) {
    return res.primFillColor(1,0,0,1,dr)
  }
  res.blue = function (dr) {
    return res.primFillColor(0,0,1,1,dr)
  }
  // In radians
  res.rotate = function(a,dr) {
    return res.primTransf(Math.cos(a), 0 - Math.sin(a), Math.sin(a), Math.cos(a), 0, 0, dr)
  }
  // In radians
  res.rotateT = function(r,dr) {
    return res.rotate(r * Math.PI*2, dr)
  }
  res.randCol = function (dr) {
    return res.primFillColor(Math.random(),Math.random(),Math.random(),1,dr)
  }
  res.randPosRect = function() {
    return res.primRect(Math.floor(Math.random()*dims.x),Math.floor(Math.random()*dims.y),30,30)
  }
  res.randPosCircle = function() {
    return res.primCircle(Math.floor(Math.random()*dims.x),Math.floor(Math.random()*dims.y),30)
  }
  return res
}

function enumFromZeroTo(n) {
  return [...Array(n).keys()]
}
function replicate(n,x) {
  return enumFromZeroTo(n).map(d => x)
}
function replicateM(n,x) {
  return enumFromZeroTo(n).map(d => x())
}





var fastDrawing = -1
var fastRenderer = null
var fastContext = null
var fastTrans = 0.0
var fastFrames = 0
function setupFast () {
  console.log("Starting fast rendering")
  var canvas = document.getElementById('canvas');

  fastContext = canvas.getContext('2d');
  // WebGL2D.enable(canvas);
  // fastContext = canvas.getContext('webgl-2d');

  fastRenderer = createRenderer(fastContext)

// > writeFile "/tmp/lubeck/test1.svg" $ unpackStr $ toSvgStr mempty
// $ fillColor C.blue $ mconcat [translateX 200 $ scale 100 circle, fillColor C.red $ scale 200 circle]

  r = fastRenderer
  fastDrawing =
    r.ap(replicateM(100,_ =>
    r.blue(r.ap(
        [ r.translateX(0,r.scale(1,r.randPosRect()))
        , r.red(r.scale(1,r.randPosRect()))
        , r.red(r.scale(Math.random(),r.randPosRect()))
        , r.randCol(r.scale(2,r.randPosRect()))
        , r.red(r.scale(Math.random(),r.randPosRect()))
        , r.red(r.scale(Math.random(),r.randPosRect()))
        , r.red(r.scale(Math.random(),r.randPosCircle()))
        , r.red(r.scale(Math.random(),r.randPosRect()))
        , r.red(r.scale(Math.random(),r.randPosRect()))
        , r.red(r.scale(Math.random(),r.randPosRect()))
        , r.red(r.scale(Math.random(),r.randPosCircle()))
      ]))))

  // r.primFillColor(Math.random()*0.8+0.2,0.2,0.2,1.3,
  //   r.ap(
  //    enumFromZeroTo(nElems).map(function (n) {
  //       return
  //        r.scale(20,1,
  //         //  r.primCircle(Math.floor(Math.random()*dims.x),Math.floor(Math.random()*dims.y),3)
  //          r.primRect(Math.floor(Math.random()*dims.x),Math.floor(Math.random()*dims.y),30,40)
  //         )
  //     })
  //   ))
}
function loopFast () {
    fastTrans = fastFrames * 2/60

    fastContext.clearRect(0.0, 0, dims.x, dims.y);
    fastRenderer.render(0,
      fastRenderer.translate(dims.x/2, dims.y/2,
        fastRenderer.rotateT(fastTrans,
            fastRenderer.translate(-dims.x/2, -dims.y/2, fastDrawing)
          )
        )
      )
    // console.log('Frame')
    if (fastFrames++ < nMaxFrames)
    requestAnimationFrame(loopFast)
}
setupFast()
loopFast()





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
