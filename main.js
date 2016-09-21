
/*
  A renderer wraps a Canvas API drawing context which may render to an on-screen canvas or an
  off-screen buffer.

  Create a renderer using createRenderer(canvasApiContext).

  Each renderer renders drawings, which are immutable trees (i.e. other primitive shapes
  or branches referencing other drawings)

  Each renderer exposes commands to create new drawings and a function render(options,drawing),
  which immediatly renders the given drawing to the underlying drawing context.

  You can have multiple renderers, but drawings can not be shared between renderers.

  ## Memory management

  - Drawings can be claimed and released with the methods 'claim' and 'release'

      renderer.claim(drawing)
      renderer.release(drawing)

  - All drawins returned from functions are claimed for the called.

      RIGHT
        var r = renderer.rectangle(...)
        renderer.release(r)

      RIGHT
        var r = renderer.rectangle(...)
        renderer.claim(r)
        renderer.release(r)
        renderer.release(r)

      WRONG
        var r = renderer.rectangle(...)
        renderer.claim(r)
        renderer.release(r)

      WRONG
        var r = renderer.rectangle(...)
        // no release

  - All functions that takes drawings as arguments implicitly claim the arguments.

      RIGHT
        var r = renderer.rectangle(...)
        var t = renderer.rotate(..., r)
        renderer.release(r)
        renderer.release(t)

      WRONG
        var r = renderer.rectangle(...
        var t = renderer.rotate(..., r)
        renderer.release(t)

      WRONG
        var r = renderer.rectangle(...
        renderer.claim(r)
        var t = renderer.rotate(..., r)
        renderer.release(r)
        renderer.release(t)


  ## Heap layout

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

#define HEAP_SIZE        0x10000
// #define HEAP_SIZE        0x1000000
// This buffer is used to return color values to the underlying context (as UTF8 strings).
#define HEAP_COLOR_BUFFER_OFFSET 0
// This region is not currently used
#define HEAP_UNUSED_OFFSET       36
// This region stores the tuples. Its size is (heap size - HEAP_TUPLES_OFFSET).
#define HEAP_TUPLES_OFFSET       36
// 0x1000
// 4096


// Indicates that a slot is free and can be re-used by the allocator
#define NODE_TYPE_FREE            0
// Primitives
#define NODE_TYPE_CIRCLE          1
#define NODE_TYPE_RECT            2

// Fill/stroke the text stored in the given text buffer slot
#define NODE_TYPE_TEXT            4
// Draw the image stored in the given image buffer slot
#define NODE_TYPE_IMAGE           5

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
// Largest possible node value
// Also serves as mask for node values, so we can reuse remaining bits for GC tags etc
#define NODE_TYPE_MAX_VALUE       0xfff
// 4095

// Misc
#define STYLE_LINE_CAP_BUTT        0
#define STYLE_LINE_CAP_ROUND       1
#define STYLE_LINE_CAP_SQUARE      2
#define STYLE_LINE_CAP_BEVEL       0
#define STYLE_LINE_CAP_ROUND       1
#define STYLE_LINE_CAP_METER       2

#define ERROR_TYPE_UNKNOWN         0
#define ERROR_TYPE_UNKNOWN_RELEASE 2
#define ERROR_OUT_OF_MEMORY        1
#define ERROR_TYPE_FREE_PASSED_TO_RENDER 3

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
  var _stroke = foreign.stroke;
  var _fillRect = foreign.fillRect;
  var _strokeRect = foreign.strokeRect;
  var _fillStyleRGBA = foreign.fillStyleRGBA;
  var _fillStyleFromColorBuffer = foreign.fillStyleFromColorBuffer
  var _strokeStyleFromColorBuffer = foreign.strokeStyleFromColorBuffer
  var _lineWidth = foreign.lineWidth
  var _lineCap = foreign.lineCap
  var _lineJoin = foreign.lineJoin
  var _lineDash = foreign.lineDash

  var _arc = foreign.arc;
  // var _rect = foreign.rect;
  var _save = foreign.save;
  var _restore = foreign.restore;
  var _transform = foreign.transform;
  var _floor = stdlib.Math.floor;
  var _imul = stdlib.Math.imul;
  var _max = stdlib.Math.max;
  var _min = stdlib.Math.min;


  var tuplesCreated = 0
  var renderingStateSetupDone = 0
  var outOfMemoryReported = 0

  function slotIndexToPtr(i) {
    i = i|0
    return (((i * 8) << 2) + HEAP_TUPLES_OFFSET) |0
  }

  // Return pointer to the first slot as a pointer (byte offset)
  // Add slot count to this, so for slot n in pointer p, use [(p + (n<<2)) >> 2]
  function newTuple() {
    // TODO this one just allocates till we run out of memory
    // Switch to scanning allocater when needed
    var max = 0
    max = getMaxNumberOfTuples()|0;
    if ((tuplesCreated|0) < (max|0)) {
      return allocateTupleInitPhase()|0
    } else {
      return allocateTupleScanning()|0
    }
    // Never happens:
    return 0|0
  }

  function allocateTupleInitPhase() {
    // Treet first 8 bytes in
    var next = 0;
    next = tuplesCreated
    tuplesCreated = tuplesCreated + 1|0
    // return (next * 4)|0
    return (((next * 8) << 2) + HEAP_TUPLES_OFFSET) |0
  }

  function allocateTupleScanning() {
    var max = 0
    var i = 0
    // var j = 0
    var ptr = 0
    max = getMaxNumberOfTuples()|0
    while ( (i|0) < (max|0)) {
      // j = i
      // Check that slot is free
      ptr = slotIndexToPtr(i)|0
      if ( ((HEAP32[ptr >> 2])|0) == NODE_TYPE_FREE ) {
        // We succeeded, so reset this state to report next time we
        // run out of memory
        outOfMemoryReported = 0
        // lastFoundTupleSlot = j // Start searching here next time
        return ptr|0
      }
      i = (i + 1)|0
    }
    // Handle out of memory state
    if (!outOfMemoryReported) {
      _debug(ERROR_OUT_OF_MEMORY)
      outOfMemoryReported = 1
    }
    return 0xffffffff|0
  }



  function getCurrentTuples() {
    return tuplesCreated|0;
  }
  function getMaxNumberOfTuples() {
    var tupleBytes = 0
    tupleBytes = (HEAP_SIZE - HEAP_TUPLES_OFFSET)|0;
    return ((tupleBytes|0)/(32|0))|0;
  }

  function getPtrType(ptr) {
    ptr = ptr|0
    var slot1 = 0
    slot1 = HEAP32[(ptr+(0<<2)) >> 2]|0
    return (slot1 & NODE_TYPE_MAX_VALUE)|0
  }

  function addToRefCount(ptr,val) {
    ptr = ptr|0
    val = val|0 // TODO verify this works for signed things

    var rc = 0
    rc = getRefCount(ptr)|0
    setRefCount(ptr, (rc + val)|0 )
    return
  }
  function setRefCount(ptr,val) {
    ptr = ptr|0
    val = val|0

    var type = 0
    type = getPtrType(ptr)|0;
    HEAP32[(ptr+(0<<2)) >> 2] = ((val << 12) | type);

    // TODO
    // ptr|0xfff
    return
  }
  function getRefCount(ptr) {
    ptr = ptr|0

    var slot1 = 0
    slot1 = HEAP32[(ptr+(0<<2)) >> 2]|0

    // TODO
    // ptr|0xfff
    return (slot1 >> 12)|0;
  }

  // Note that new nodes have a refcount of 0

  // We take this to mean EXTERNAL references, so nodes are reclaimed
  // if release() is called when their ref count is 0

  // That means, no need to call claim on NEW nodes, but it must be called
  // on every argument to a node constructor.

  function claim(ptr) {
    ptr = ptr|0
    addToRefCount(ptr, 1)
    return
  }

  function releaseChildren(dr) {
    dr = dr|0

    var drType = 0
    // var ai = 0
    // var x = 0.
    // var y = 0.
    // var w = 0.
    // var h = 0.
    // var r = 0.
    // var a = 0.
    // var b = 0.
    // var c = 0.
    // var d = 0.
    // var e = 0.
    // var f = 0.
    // // var r = 0.
    // var g = 0.
    // // var b = 0.
    var dr1 = 0
    var dr2 = 0


    drType = getPtrType(dr)|0;

    switch (drType|0) {

      case NODE_TYPE_FREE:
        // FIXME should we really run into free nodes here?
        break;
      case NODE_TYPE_CIRCLE:
        break;
      case NODE_TYPE_RECT:
        break;
      case NODE_TYPE_FILL_COLOR:
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        release(dr1);
        break;

      case NODE_TYPE_STROKE_COLOR:
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        release(dr1);
        break;

      case NODE_TYPE_LINE_WIDTH:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        break;
      case NODE_TYPE_TRANSF:
        dr1 = HEAP32[(dr+(7<<2)) >> 2]|0;
        // FIXME this line breaks tests
        release(dr1);
        break;

      case NODE_TYPE_AP2:
        dr1 = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr2 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        release(dr2);
        break;

      default:
        _debug(ERROR_TYPE_UNKNOWN_RELEASE);
        break;
    }
  }

  function release(ptr) {
    ptr = ptr|0
    var rc = 0
    rc = getRefCount(ptr)|0
    if ((rc|0) == 0) {
        // Release sub-nodes here (depends on type)
        releaseChildren(ptr)
        // Mark the slot as free
        // FIXME this seems to cause bugs
        // HEAP32[(ptr+(0<<2)) >> 2] = NODE_TYPE_FREE
    } else {
      addToRefCount(ptr, -1)
    }

    return
  }

    // TODO fix this and remaining warnings, push
    // TODO solve preprocessor line offset (so we can still use FF for validation)
    // TODO add remaining styles + test
    // TODO add remaining shapes + test
    // TODO text (probably best if this is never transmitted into the render (i.e. use external map and let
    // text nodes include indices))
    // TODO test with Lubeck
    // TODO basic GC
    // TODO event detection (tag single node and map position to that)

  // Return offset of the string buffer as a pointer (byte offset)
  function getStringBufferOffset() {
    return 0
  }


  function drawCircle(x,y,r,hasFill,hasStroke) {
    x=+x
    y=+y
    r=+r
    hasFill = hasFill|0;
    hasStroke = hasStroke|0;

    // TODO optimize away fill/stroke if the appropriate color is not set
    _beginPath();
    _arc(x,y,r,0, 6.283185307179586,0/*false*/);
    if (hasFill) {
      _fill();
    }
    if (hasStroke) {
      _stroke();
    }
  }
  function drawRect(x,y,w,h,hasFill,hasStroke) {
    x=+x
    y=+y
    w=+w
    h=+h
    hasFill = hasFill|0;
    hasStroke = hasStroke|0;
    // _rect(x,y,w,h);
    // _fill();

    if (hasFill) {
      _fillRect(x,y,w,h);
    }
    if (hasStroke) {
      _strokeRect(x,y,w,h);
    }
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

    // Filter out numbers outside [0..1], multiply by 256 and round down
    n2 = ~~_floor( _max(0.,_min(n,1.)) * 256.0 )|0 // n2 :: signed

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
  function writeAlphaString(ptr,n) {
    ptr = ptr|0
    n = +n

    var n2 = 0
    var d1 = 0
    var d2 = 0
    var d3 = 0

    // Filter out numbers outside [0..1], multiply by 100 and round down
    n2 = ~~_floor( _max(0.,_min(n,1.)) * 100.0 )|0 // n2 :: signed

    // Then calculate digits (div by 10 and 100)
    d1 = (((n2|0) / (100|0))|0) % (10|0) | 0 // (n2 / 100) % 10
    d2 = (((n2|0) / (10 |0))|0) % (10|0) | 0 // (n2 / 10 ) % 10
    d3 = (((n2|0) / (1  |0))|0) % (10|0) | 0 // (n2 / 1  ) % 10

    // Then convert digits to char codes and write to ptr
    HEAPU8[(ptr + 0) >> 0] = 48 + d1
    HEAPU8[(ptr + 1) >> 0] = 46 // '.'
    HEAPU8[(ptr + 2) >> 0] = 48 + d2
    HEAPU8[(ptr + 3) >> 0] = 48 + d3
  }

  function setupColorBuffer() {
    HEAPU8 [(0 + (0<<0)) >> 0] = 114 // 'r'
    HEAPU8 [(0 + (1<<0)) >> 0] = 103 // 'g'
    HEAPU8 [(0 + (2<<0)) >> 0] = 98 // 'b'
    HEAPU8 [(0 + (3<<0)) >> 0] = 97 // 'a'
    HEAPU8 [(0 + (4<<0)) >> 0] = 40 // '('
    // writeColorString(5, r)
    HEAPU8 [(0 + (8<<0)) >> 0] = 44 // ','
    // writeColorString(9, g)
    HEAPU8 [(0 + (12<<0)) >> 0] = 44 // ','
    // writeColorString(13, b)
    HEAPU8 [(0 + (16<<0)) >> 0] = 44 // ','
    // writeAlphaString(17, a)
    HEAPU8 [(0 + (21<<0)) >> 0] = 41 // ')'
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
    // HEAPU8 [(0 + (0<<0)) >> 0] = 114 // 'r'
    // HEAPU8 [(0 + (1<<0)) >> 0] = 103 // 'g'
    // HEAPU8 [(0 + (2<<0)) >> 0] = 98 // 'b'
    // HEAPU8 [(0 + (3<<0)) >> 0] = 97 // 'a'
    // HEAPU8 [(0 + (4<<0)) >> 0] = 40 // '('
    writeColorString(5, r)
    // HEAPU8 [(0 + (8<<0)) >> 0] = 44 // ','
    writeColorString(9, g)
    // HEAPU8 [(0 + (12<<0)) >> 0] = 44 // ','
    writeColorString(13, b)
    // HEAPU8 [(0 + (16<<0)) >> 0] = 44 // ','
    writeAlphaString(17, a)
    // HEAPU8 [(0 + (21<<0)) >> 0] = 41 // ')'

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

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_FILL_COLOR|0
    HEAPF32[(p + (1<<2)) >> 2] = r
    HEAPF32[(p + (2<<2)) >> 2] = g
    HEAPF32[(p + (3<<2)) >> 2] = b
    HEAPF32[(p + (4<<2)) >> 2] = a
    HEAP32 [(p + (5<<2)) >> 2] = dr
    return p|0
  }
  // Double ^ 4 -> Drawing*
  function primStrokeColor(r,g,b,a,dr) {
    r = +r;
    g = +g;
    b = +b;
    a = +a;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_STROKE_COLOR|0
    HEAPF32[(p + (1<<2)) >> 2] = r
    HEAPF32[(p + (2<<2)) >> 2] = g
    HEAPF32[(p + (3<<2)) >> 2] = b
    HEAPF32[(p + (4<<2)) >> 2] = a
    HEAP32 [(p + (5<<2)) >> 2] = dr
    return p|0
  }
  // Double ^ 4 -> Drawing*
  function primLineWidth(a,dr) {
    a = +a;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_LINE_WIDTH|0
    HEAPF32[(p + (1<<2)) >> 2] = a
    HEAP32 [(p + (2<<2)) >> 2] = dr
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

    claim(dr)

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
  // Draws dr2 above dr1
  function primAp2(dr1,dr2) {
    dr1 = dr1|0;
    dr2 = dr2|0;

    var p = 0

    claim(dr1)
    claim(dr2)

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



  function setupRenderingState() {
    setupColorBuffer()
  }

  // Render without checking that setupRenderingState has been called
  // Opts* -> Drawing* -> ()
  function renderWithoutCheck(opts,dr,hasFill,hasStroke) {
    opts = opts|0;
    dr = dr|0;
    hasFill = hasFill|0;
    hasStroke = hasStroke|0;

    var drType = 0
    // var ai = 0
    // var x = 0.
    // var y = 0.
    // var w = 0.
    // var h = 0.
    // var r = 0.

    var a = 0.
    var b = 0.
    var c = 0.
    var d = 0.
    var e = 0.
    var f = 0.
    // var r = 0.
    // var g = 0.
    // var b = 0.
    var dr1 = 0
    var dr2 = 0

    // var i = 0
    var cont = 0

    do {
    cont = 0
    drType = getPtrType(dr)|0;

    /*
    Invariant:
      Each call to renderWithoutCheck() that returns normally MUST leave the state of the underlying
      drawing context exactly as it found it. As this function may call itself recursively, this
      means that states have to be stored in a stack somewhere.

      Options include
        - Using the drawing context stack, i.e. calling save(), doing anything it wants and then call restore()
        - Using a custom stack structure on the renderer heap, similar to the save()/restore() option above
        - Using the call stack, i.e. storing the previous value in a variable, updating drawing context, drawing, then restoring
          the drawing context from the variable.

    */
    switch (drType|0) {

      case NODE_TYPE_CIRCLE:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        drawCircle(a,b,c,hasFill,hasStroke)
        // console.log("Rendering circle: ", x, y, r)
        break;

      case NODE_TYPE_RECT:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        drawRect(a,b,c,d,hasFill,hasStroke)
        // console.log("Rendering circle: ", x, y, r)
        break;

      case NODE_TYPE_FILL_COLOR:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        // console.log("Rendering fill: ", r, g, b, a)
        // FIXME selective version of save/restore
        _save()

        // _fillStyleRGBA(r,g,b,a)
        writeRGBAStringToBuffer(a,b,c,d)
        _fillStyleFromColorBuffer()

        renderWithoutCheck(opts,dr1,1,hasStroke) // render with hasFill set
        _restore()
        break;

      case NODE_TYPE_STROKE_COLOR:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        // console.log("Rendering fill: ", r, g, b, a)
        // FIXME selective version of save/restore
        _save()

        // _fillStyleRGBA(r,g,b,a)
        writeRGBAStringToBuffer(a,b,c,d)
        _strokeStyleFromColorBuffer()

        renderWithoutCheck(opts,dr1,hasFill,1) // render with hasStroke set
        _restore()
        break;

      case NODE_TYPE_LINE_WIDTH:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        _save()
        _lineWidth(a);
        renderWithoutCheck(opts,dr1,hasFill,hasStroke)
        _restore()
        break;
      //
      // case NODE_TYPE_LINE_CAP:
      //   a = +HEAPF32[(dr+(1<<2)) >> 2];
      //   dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
      //   _save()
      //   _lineCap(a);
      //   renderWithoutCheck(opts,dr1)
      //   _restore()
      //   break;
      //
      // case NODE_TYPE_LINE_JOIN:
      //   a = +HEAPF32[(dr+(1<<2)) >> 2];
      //   dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
      //   _save()
      //   _lineJoin(a);
      //   renderWithoutCheck(opts,dr1)
      //   _restore()
      //   break;
      //
      // case NODE_TYPE_LINE_DASH:
      //   ai = HEAP32[(dr+(1<<2)) >> 2]|0; // Number of slots used
      //   b = +HEAPF32[(dr+(2<<2)) >> 2]; // Slots 2-6 store the actual values
      //   c = +HEAPF32[(dr+(3<<2)) >> 2];
      //   d = +HEAPF32[(dr+(4<<2)) >> 2];
      //   e = +HEAPF32[(dr+(5<<2)) >> 2];
      //   f = +HEAPF32[(dr+(6<<2)) >> 2];
      //   dr1 = HEAP32[(dr+(7<<2)) >> 2]|0;
      //   _save()
      //   _lineDash(a,b,c,d,e,f);
      //   render(opts,dr1)
      //   _restore()
      //   break;

      // TODO figure out a good heap repr for linear gradients
      // I.e. one tuple for (x0, y0, x1, y1, stops, dr)
      // The stops is some kind of list of colors (percent,r,g,b,a)



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
        renderWithoutCheck(opts,dr1,hasFill,hasStroke)
        _restore()
        break;

      case NODE_TYPE_AP2:
        dr1 = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr2 = HEAP32[(dr+(2<<2)) >> 2]|0;

        renderWithoutCheck(opts,dr1,hasFill,hasStroke)

        // render(opts,dr2)
        // Manual tail-call opt: Instead of calling this function again, we update the parameters and set cont to true
        opts = opts
        dr   = dr2
        cont = 1

        break;

      case NODE_TYPE_FREE:
        _debug(ERROR_TYPE_FREE_PASSED_TO_RENDER);
        break;
      default:
        _debug(ERROR_TYPE_UNKNOWN);
        break;
    }
    }
    while(cont);
  }


  // Opts* -> Drawing* -> ()
  function render(opts,dr) {
    opts = opts|0;
    dr = dr|0;

    if (!renderingStateSetupDone) {
      setupRenderingState()
      renderingStateSetupDone = 1
    }
    renderWithoutCheck(opts,dr,0,0)
  }

  // etc
  return  { render : render
          , primCircle : primCircle
          , primRect : primRect
          , primFillColor : primFillColor
          , primStrokeColor : primStrokeColor
          , primLineWidth : primLineWidth
          , primTransf : primTransf
          , primAp2 : primAp2
          , getMaxNumberOfTuples : getMaxNumberOfTuples
          , getCurrentTuples : getCurrentTuples

          , slotIndexToPtr : slotIndexToPtr
          , getPtrType : getPtrType
          , addToRefCount : addToRefCount
          , getRefCount : getRefCount

          , newTuple : newTuple
          , allocateTupleInitPhase : allocateTupleInitPhase
          , allocateTupleScanning : allocateTupleScanning

          , claim : claim
          , release : release
          }
}

function createRenderer(c2) {
  const c = c2

  var heap        = new ArrayBuffer(HEAP_SIZE)

  var colorBuffer = new Uint8Array(heap, HEAP_COLOR_BUFFER_OFFSET, 22);
  var utf8d       = new TextDecoder("utf-8");

  // ASM module is linked here...
  var res = new AsmDrawingRenderer(window,
      { beginPath:
        function (x) { c.beginPath() }
        // x=>console.log('beginPath')
      , fill:
        // x=>console.log('fill')
        function (x) { c.fill() }
      , stroke:
        // x=>console.log('fill')
        function (x) { c.stroke() }
      , fillStyleFromColorBuffer:
        function () {
          c.fillStyle = utf8d.decode(colorBuffer)
        }
      , strokeStyleFromColorBuffer:
      function () {
        c.strokeStyle = utf8d.decode(colorBuffer)
      }
      , lineWidth:
      function (x) {
        c.lineWidth = x
      }
      , lineCap:
      function (x) {
        c.lineCap = 0 // FIXME
      }
      , lineJoin:
      function (x) {
        c.lineJoin = 0 // FIXME
      }
      , lineDash:
      function (x) {
        c.lineDash = 0 // FIXME
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
      , strokeRect:
      // x=>console.log('x')
        function (x,y,w,h) {
          x = +x
          y = +y
          w = +w
          h = +h
          c.strokeRect(x,y,w,h)
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
        function (msg) {
          // FIXME stop rendering somehow
          switch (msg) {
            case ERROR_TYPE_UNKNOWN:
              console.log("Error: ", "Unknown node type")
              break;
            case ERROR_TYPE_UNKNOWN_RELEASE:
              console.log("Error: ", "Unknown node type (release)")
              break;
            case ERROR_OUT_OF_MEMORY:
              console.log("Error: ", "Out of memory")
              break;
            case ERROR_TYPE_FREE_PASSED_TO_RENDER:
              console.log("Error: ", "Free slot passed to render")
              break;
            default:
              console.log("Error: ", "(unknown)")
              break;
          }
         }
      }, heap) // FIXME trim

  // Some helpers

  res.ap = function(xs) {
    // TODO primitive empty drawing
    var empty = r.primRect(0,0,0,0)
    var res = xs.reduce(function (a,b) {
      return r.primAp2(b,a) // Reverse order
    }, empty)
    return res
  }
  res.empty = function () {
    return res.ap([])
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
  res.redA = function (dr) {
    return res.primFillColor(1,0,0,0.5,dr)
  }
  res.blueA = function (dr) {
    return res.primFillColor(0,0,1,0.5,dr)
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

  // TODO this could also be triggered by a special node in the drawing tree
  res.dumpHeap = function () {
    console.log("Heap size:", HEAP_SIZE)
    console.log("Max tuples:", res.getMaxNumberOfTuples())
    console.log("Current tuples:", res.getCurrentTuples())
    console.log("Dumping heap below...")
    console.log("")
    console.log("Color buffer:", utf8d.decode(colorBuffer))
    res.dumpTuples()
  }
  res.dumpTuples = function () {
    var n = res.getMaxNumberOfTuples()
    for (var i = 0; i < n; ++i) {
      var ptr = res.slotIndexToPtr(i)
      console.log("Tuple at address", ptr)
      console.log("  ", "Type:", res.getPtrType(ptr))
      console.log("  ", "(External) references:", res.getRefCount(ptr))
    }
  }

  // TODO debug
  window.globalRenderer = res
  return res
}
