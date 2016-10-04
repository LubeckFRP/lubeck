
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

// Heap bounds

#define HEAP_SIZE                0x1000000
// This buffer is used to return color values to the underlying context (as UTF8 strings).
#define HEAP_COLOR_BUFFER_OFFSET 0
// This region is not currently used
#define HEAP_UNUSED_OFFSET       36
// This region stores the tuples. Its size is (heap size - HEAP_TUPLES_OFFSET).
#define HEAP_TUPLES_OFFSET       36


// Node types

// Indicates that a slot is free and can be re-used by the allocator
#define NODE_TYPE_FREE            0
// Primitives
#define NODE_TYPE_CIRCLE          1
#define NODE_TYPE_RECT            2
#define NODE_TYPE_TEXT            3
#define NODE_TYPE_PATH            4

// Affine transformations
#define NODE_TYPE_TRANSF          63

// Styles
#define NODE_TYPE_FILL_COLOR      64
#define NODE_TYPE_FILL_GRADIENT   65
#define NODE_TYPE_FILL_PATTERN    66
#define NODE_TYPE_STROKE_COLOR    67
#define NODE_TYPE_LINE_WIDTH      68
#define NODE_TYPE_LINE_CAP        69
#define NODE_TYPE_LINE_JOIN       70
#define NODE_TYPE_TEXT_FONT       71
#define NODE_TYPE_TEXT_ALIGNMENT  72
#define NODE_TYPE_TEXT_BASELINE   73

#define NODE_TYPE_TAG             127

// Groups (named for the verb "append")
#define NODE_TYPE_AP2             128
#define NODE_TYPE_AP3             129
#define NODE_TYPE_AP4             130
#define NODE_TYPE_CLIP            131

// Segments
#define NODE_TYPE_SEGMENT         256
#define NODE_TYPE_SEGMENT2        257
#define NODE_TYPE_SEGMENT3        258
#define NODE_TYPE_SEGMENT_ARC     259
#define NODE_TYPE_SEGMENT_END     260
#define NODE_TYPE_SEGMENT_SUBPATH 261

// Largest possible node value
// Also serves as mask for node values, so we can reuse remaining bits for GC tags etc
#define NODE_TYPE_MAX_VALUE       0xfff


// Enumerations

#define STYLE_TEXT_ALIGN_START      0
#define STYLE_TEXT_ALIGN_END        1
#define STYLE_TEXT_ALIGN_LEFT       2
#define STYLE_TEXT_ALIGN_RIGHT      3
#define STYLE_TEXT_ALIGN_CENTER     4

#define STYLE_TEXT_BASELINE_TOP           0
#define STYLE_TEXT_BASELINE_HANGING       1
#define STYLE_TEXT_BASELINE_MIDDLE        2
#define STYLE_TEXT_BASELINE_ALPHABETIC    3
#define STYLE_TEXT_BASELINE_IDEOGRAPHIC   4
#define STYLE_TEXT_BASELINE_BOTTOM        5

#define STYLE_LINE_CAP_BUTT        0
#define STYLE_LINE_CAP_ROUND       1
#define STYLE_LINE_CAP_SQUARE      2

#define STYLE_LINE_JOIN_BEVEL      0
#define STYLE_LINE_JOIN_ROUND      1
#define STYLE_LINE_JOIN_MITER      2

#define ERROR_TYPE_UNKNOWN         0
#define ERROR_TYPE_UNKNOWN_RELEASE 2
#define ERROR_OUT_OF_MEMORY        1
#define ERROR_TYPE_FREE_PASSED_TO_RENDER 3
#define ERROR_NO_TAG               0
#define ERROR_POINT_OUTSIDE        1
// Minimal value for tags
#define TAG_OFFSET                 2

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

  var _printCurrentPath = foreign.printCurrentPath
  var _beginPath = foreign.beginPath
  var _moveTo = foreign.moveTo
  var _closePath = foreign.closePath
  var _lineTo = foreign.lineTo
  var _quadraticCurveTo = foreign.quadraticCurveTo
  var _bezierCurveTo = foreign.bezierCurveTo
  var _arcTo = foreign.arcTo
  var _arc = foreign.arc
  var _rect = foreign.rect

  var _fill = foreign.fill
  var _stroke = foreign.stroke
  var _fillRect = foreign.fillRect
  var _strokeRect = foreign.strokeRect
  var _fillText = foreign.fillText

  var _fillStyleRGBA = foreign.fillStyleRGBA;
  var _fillStyleFromColorBuffer = foreign.fillStyleFromColorBuffer
  var _strokeStyleFromColorBuffer = foreign.strokeStyleFromColorBuffer
  var _fillGradient = foreign.fillGradient
  var _fillPattern = foreign.fillPattern
  var _font = foreign.font
  var _textAlign = foreign.textAlign
  var _textBaseline = foreign.textBaseline
  var _lineWidth = foreign.lineWidth
  var _lineCap = foreign.lineCap
  var _lineJoin = foreign.lineJoin
  var _releaseExternal = foreign.releaseExternal

  // var _rect = foreign.rect;
  var _setTransform = foreign.setTransform
  var _save = foreign.save;
  var _restore = foreign.restore;
  var _transform = foreign.transform;

  var _isPointInPath = foreign.isPointInPath

  var _floor = stdlib.Math.floor;
  var _imul = stdlib.Math.imul;
  var _max = stdlib.Math.max;
  var _min = stdlib.Math.min;
  var _random = foreign.random;


  var tuplesCreated = 0
  var allocationsPerformed = 0
  var deallocationsPerformed = 0
  var renderingStateSetupDone = 0
  var outOfMemoryReported = 0

  function slotIndexToPtr(i) {
    i = i|0
    return (((i * 8) << 2) + HEAP_TUPLES_OFFSET) |0
  }

  function getAllocationInfo(type) {
    type = type|0
    if (type) {
      return allocationsPerformed|0
    } else {
      return deallocationsPerformed|0
    }
    // Unreachable
    return 0|0
  }

  // Return pointer to the first slot as a pointer (byte offset)
  // Add slot count to this, so for slot n in pointer p, use [(p + (n<<2)) >> 2]
  function newTuple() {
    allocationsPerformed = (allocationsPerformed + 1)|0
    return allocateTupleScanning()|0
  }

  // function allocateTupleInitPhase() {
  //   // Treet first 8 bytes in
  //   var next = 0;
  //   next = tuplesCreated
  //   tuplesCreated = tuplesCreated + 1|0
  //   // return (next * 4)|0
  //   return slotIndexToPtr(next)|0
  // }

  function allocateTupleScanning() {
    var max = 0
    var maxF = 0.0
    var i = 0
    // var j = 0
    var ptr = 0
    var j = 0
    var startingPoint = 0
    max = getMaxNumberOfTuples()|0
    maxF = +(max|0)
    startingPoint = ((~~_floor((+_random()) * (+maxF))) % (max|0))|0 // Random position in [0..max]
    while ( (i|0) < (max|0)) {
      j = (((i + startingPoint)|0) % (max|0))|0
      // j = i
      // Check that slot is free
      ptr = slotIndexToPtr(j)|0
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
      _debug(ERROR_OUT_OF_MEMORY, 0)
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
    val = val|0

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

    return
  }
  function getRefCount(ptr) {
    ptr = ptr|0

    var slot1 = 0
    slot1 = HEAP32[(ptr+(0<<2)) >> 2]|0

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
    var dr1 = 0
    var dr2 = 0
    var ext = 0


    drType = getPtrType(dr)|0;

    switch (drType|0) {

      case NODE_TYPE_FREE:
        break;

      case NODE_TYPE_CIRCLE:
        break;
      case NODE_TYPE_RECT:
        break;
      case NODE_TYPE_TEXT:
        ext = HEAP32 [(dr + (3<<2)) >> 2]|0
        _releaseExternal(ext|0)
        break;
      case NODE_TYPE_PATH:
        dr1 = HEAP32[(dr+(3<<2)) >> 2]|0;
        release(dr1);
        break;

      case NODE_TYPE_TRANSF:
        dr1 = HEAP32[(dr+(7<<2)) >> 2]|0;
        release(dr1);
        break;


      case NODE_TYPE_FILL_COLOR:
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        release(dr1);
        break;
      case NODE_TYPE_FILL_GRADIENT:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        break;
      case NODE_TYPE_FILL_PATTERN:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
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
      case NODE_TYPE_LINE_CAP:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        break;
      case NODE_TYPE_LINE_JOIN:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        break;
      case NODE_TYPE_TEXT_FONT:
        ext = HEAP32 [(dr + (1<<2)) >> 2]|0
        dr1 = HEAP32 [(dr + (2<<2)) >> 2]|0;
        _releaseExternal(ext|0)
        release(dr1);
        break;
      case NODE_TYPE_TEXT_ALIGNMENT:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        break;
      case NODE_TYPE_TEXT_BASELINE:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        break;

      case NODE_TYPE_TAG:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        break;

      case NODE_TYPE_AP2:
        dr1 = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr2 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        release(dr2);
        break;
      case NODE_TYPE_CLIP:
        dr1 = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr2 = HEAP32[(dr+(2<<2)) >> 2]|0;
        release(dr1);
        release(dr2);
        break;

      case NODE_TYPE_SEGMENT:
        dr1 = HEAP32[(dr+(3<<2)) >> 2]|0; // segs
        release(dr1);
        break;
      case NODE_TYPE_SEGMENT2:
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0; // segs
        release(dr1);
        break;
      case NODE_TYPE_SEGMENT3:
        dr1 = HEAP32[(dr+(7<<2)) >> 2]|0; // segs
        release(dr1);
        break;
      case NODE_TYPE_SEGMENT_ARC:
        dr1 = HEAP32[(dr+(6<<2)) >> 2]|0; // segs
        release(dr1);
        break;
      case NODE_TYPE_SEGMENT_END:
        break;
      case NODE_TYPE_SEGMENT_SUBPATH:
        dr1 = HEAP32[(dr+(4<<2)) >> 2]|0; // segs
        release(dr1);
        break;

      default:
        _debug(ERROR_TYPE_UNKNOWN_RELEASE, drType|0);
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
        // Notify deallocation succeeded
        deallocationsPerformed = (deallocationsPerformed + 1)|0
        // Mark the slot as free
        HEAP32[(ptr+(0<<2)) >> 2] = NODE_TYPE_FREE
    } else {
      addToRefCount(ptr, -1)
    }
    return
  }

  // Return offset of the string buffer as a pointer (byte offset)
  function getStringBufferOffset() {
    return 0
  }


  function drawCircle(x,y,r,hasFill,hasStroke,hasClip) {
    x=+x
    y=+y
    r=+r
    hasFill = hasFill|0;
    hasStroke = hasStroke|0;
    hasClip = hasClip|0

    _beginPath();
    _arc(x,y,r,0, 6.283185307179586,0/*false*/);
    if (hasFill) {
      _fill();
    }
    if (hasStroke) {
      strokeWithoutPointWiseTransform()
    }
  }

  function drawRect(x,y,w,h,hasFill,hasStroke,hasClip) {
    x=+x
    y=+y
    w=+w
    h=+h
    hasFill = hasFill|0;
    hasStroke = hasStroke|0;
    hasClip = hasClip|0

    if (hasFill) {
      _fillRect(x,y,w,h);
    }
    if (hasStroke) {
      // Generates a path equivalent to _rect(x,y,w,h)
      // This is affected by the transformation, but the below fill is not (see longer comment )
      _beginPath()
      _moveTo(x,   y)
      _lineTo(x+w, y)
      _lineTo(x+w, y+h)
      _lineTo(x,   y+h)
      _closePath()
      strokeWithoutPointWiseTransform();
    }
  }

  function drawText(x,y,txt,hasFill,hasStroke) {
    x=+x
    y=+y
    txt=txt|0
    hasFill = hasFill|0;
    hasStroke = hasStroke|0;
    // Don't support stroke
    if (hasFill) {
      _fillText(x,y,txt|0)
    }
  }

  function strokeWithoutPointWiseTransform() {
    // Assues that the current path has been set by previous calls and
    // affected by any transformation higher up in the tree. To avoid a pointwise
    // scaling of the stroke (which we generally don't want), temporarily reset
    // the transformation here. Note that this does NOT affect the shape ByteString
    // stroke (as this has already been transmitted via path mutation calls.)
    //
    // The effect of resetting the matrix is the same as SVGs non-scaling-stroke
    //
    // For a more full explanation see here
    // http://www.bit-101.com/blog/?p=3690
    _save()
    _setTransform(1,0,0,1,0,0)
    _stroke()
    _restore()
  }

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

  // P2 -> TextRef -> Drawing*
  function primText(x, y, txt) {
    x = +x;
    y = +y;
    txt = txt|0;

    var p = 0

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_TEXT|0
    HEAPF32[(p + (1<<2)) >> 2] = x
    HEAPF32[(p + (2<<2)) >> 2] = y
    HEAP32 [(p + (3<<2)) >> 2] = txt
    return p|0
  }

  // P2 -> Segment* -> Drawing*
  function primPath(x, y, path) {
    x = +x;
    y = +y;
    path = path|0;

    var p = 0
    claim(path)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_PATH|0
    HEAPF32[(p + (1<<2)) >> 2] = x
    HEAPF32[(p + (2<<2)) >> 2] = y
    HEAP32 [(p + (3<<2)) >> 2] = path
    return p|0
  }

  function primSegment(x, y, tail) {
    x = +x;
    y = +y;
    tail = tail|0;

    var p = 0;
    claim(tail)
    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_SEGMENT|0
    HEAPF32[(p + (1<<2)) >> 2] = x
    HEAPF32[(p + (2<<2)) >> 2] = y
    HEAP32 [(p + (3<<2)) >> 2] = tail
    return p|0
  }
  function primSegment2(x1, y1, x2, y2, tail) {
    x1 = +x1;
    y1 = +y1;
    x2 = +x2;
    y2 = +y2;
    tail = tail|0;

    var p = 0;
    claim(tail)
    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_SEGMENT2|0
    HEAPF32[(p + (1<<2)) >> 2] = x1
    HEAPF32[(p + (2<<2)) >> 2] = y1
    HEAPF32[(p + (3<<2)) >> 2] = x2
    HEAPF32[(p + (4<<2)) >> 2] = y2
    HEAP32 [(p + (5<<2)) >> 2] = tail
    return p|0
  }
  function primSegment3(x1, y1, x2, y2, x3, y3, tail) {
    x1 = +x1;
    y1 = +y1;
    x2 = +x2;
    y2 = +y2;
    x3 = +x3;
    y3 = +y3;
    tail = tail|0;

    var p = 0;
    claim(tail)
    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_SEGMENT3|0
    HEAPF32[(p + (1<<2)) >> 2] = x1
    HEAPF32[(p + (2<<2)) >> 2] = y1
    HEAPF32[(p + (3<<2)) >> 2] = x2
    HEAPF32[(p + (4<<2)) >> 2] = y2
    HEAPF32[(p + (5<<2)) >> 2] = x3
    HEAPF32[(p + (6<<2)) >> 2] = y3
    HEAP32 [(p + (7<<2)) >> 2] = tail
    return p|0
  }
  function primSegmentArc(x1, y1, x2, y2, rad, tail) {
    x1 = +x1;
    y1 = +y1;
    x2 = +x2;
    y2 = +y2;
    rad = +rad;
    tail = tail|0;
    var p = 0;
    claim(tail)
    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_SEGMENT_ARC|0
    // TODO arc segment slots
    _debug(ERROR_TYPE_UNKNOWN,0)
    return p|0
  }
  function primSegmentEnd(close) {
    close = close|0;
    var p = 0;
    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_SEGMENT_END|0
    HEAP32 [(p + (1<<2)) >> 2] = close
    return p|0
  }
  function primSegmentSubpath(close, x, y, tail) {
    close = close|0;
    x = +x;
    y = +y;
    tail = tail|0;
    var p = 0;
    claim(tail)
    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_SEGMENT_SUBPATH|0
    HEAP32 [(p + (1<<2)) >> 2] = close
    HEAPF32[(p + (2<<2)) >> 2] = x
    HEAPF32[(p + (3<<2)) >> 2] = y
    HEAP32 [(p + (4<<2)) >> 2] = tail
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
  // Double -> Drawing*
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

  // Int -> Drawing* - >Drawing*
  function primLineCap(a,dr) {
    a = a|0;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_LINE_CAP|0
    HEAP32 [(p + (1<<2)) >> 2] = a
    HEAP32 [(p + (2<<2)) >> 2] = dr
    return p|0
  }

  // Int -> Drawing* - >Drawing*
  function primLineJoin(a,dr) {
    a = a|0;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_LINE_JOIN|0
    HEAP32 [(p + (1<<2)) >> 2] = a
    HEAP32 [(p + (2<<2)) >> 2] = dr
    return p|0
  }

  // Int -> Drawing* - >Drawing*
  function primTextFont(a,dr) {
    a = a|0;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_TEXT_FONT|0
    HEAP32 [(p + (1<<2)) >> 2] = a
    HEAP32 [(p + (2<<2)) >> 2] = dr
    return p|0
  }

  // Int -> Drawing* - >Drawing*
  function primTextAlignment(a,dr) {
    a = a|0;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_TEXT_ALIGNMENT|0
    HEAP32 [(p + (1<<2)) >> 2] = a
    HEAP32 [(p + (2<<2)) >> 2] = dr
    return p|0
  }

  // Int -> Drawing* - >Drawing*
  function primTextBaseline(a,dr) {
    a = a|0;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_TEXT_BASELINE|0
    HEAP32 [(p + (1<<2)) >> 2] = a
    HEAP32 [(p + (2<<2)) >> 2] = dr
    return p|0
  }

  function primFillGradient(a, dr) {
    a = a|0;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_FILL_GRADIENT|0
    HEAP32 [(p + (1<<2)) >> 2] = a
    HEAP32 [(p + (2<<2)) >> 2] = dr
    return p|0
  }
  function primFillPattern(a, dr) {
    a = a|0;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_FILL_PATTERN|0
    HEAP32 [(p + (1<<2)) >> 2] = a
    HEAP32 [(p + (2<<2)) >> 2] = dr
    return p|0
  }


  // Int -> Drawing* - >Drawing*
  function primTag(a,dr) {
    a = a|0;
    dr = dr|0;

    var p = 0

    claim(dr)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_TAG|0
    HEAP32 [(p + (1<<2)) >> 2] = a
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

    return p|0
  }

  function primClip(dr1, dr2) {
    dr1 = dr1|0;
    dr2 = dr2|0;

    var p = 0

    claim(dr1)
    claim(dr2)

    p = (newTuple())|0
    HEAP32 [(p + (0<<2)) >> 2] = NODE_TYPE_CLIP|0
    HEAP32 [(p + (1<<2)) >> 2] = dr1
    HEAP32 [(p + (2<<2)) >> 2] = dr2

    return p|0
  }



  function setupRenderingState() {
    setupColorBuffer()
  }

  /*
  Render without checking that setupRenderingState has been called

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
  // Opts* -> Drawing* -> Bool -> Bool -> Bool -> ()
  function renderWithoutCheck(opts,dr,hasFill,hasStroke,hasClip) {
    opts = opts|0;
    dr = dr|0;
    hasFill = hasFill|0;
    hasStroke = hasStroke|0;
    hasClip = hasClip|0 // TODO not actually used

    /* Don't rely on these default values as manual TCO (see below) may caused
      them to have a different value. */
    var drType = 0

    var a = 0.
    var b = 0.
    var c = 0.
    var d = 0.
    var e = 0.
    var f = 0.

    var dr1 = 0
    var dr2 = 0
    var txt = 0 // Bad name

    var cont = 0

    /*
    This loops is here to allow us to fake tail-calls (to itself).

    Convention is that instead of tail-calling renderWithoutCheck(newParams), you simply
    set the parameters appropriatly, set 'cont' to truish and then break so control jumps
    up back here, resetting 'cont' and then dispatching on the type of 'dr'.

    Obviously don't try to do this with calls that are not in tail position.
    */
    do {
    cont = 0
    drType = getPtrType(dr)|0;

    switch (drType|0) {

      case NODE_TYPE_FREE:
        _debug(ERROR_TYPE_FREE_PASSED_TO_RENDER, 0);
        break;

      case NODE_TYPE_CIRCLE:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        drawCircle(a,b,c,hasFill,hasStroke,hasClip)
        break;

      case NODE_TYPE_RECT:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        drawRect(a,b,c,d,hasFill,hasStroke,hasClip)
        break;

      case NODE_TYPE_TEXT:
        a   = +HEAPF32[(dr + (1<<2)) >> 2]
        b   = +HEAPF32[(dr + (2<<2)) >> 2]
        txt =  HEAP32 [(dr + (3<<2)) >> 2]|0 // txt

        // We don't support text for clipping paths
        drawText(a,b,txt,hasFill,hasStroke)
        break;

      case NODE_TYPE_PATH:
        a   = +HEAPF32[(dr + (1<<2)) >> 2] // x
        b   = +HEAPF32[(dr + (2<<2)) >> 2] // y
        txt =  HEAP32 [(dr + (3<<2)) >> 2]|0 // segments

        _beginPath()
        _moveTo(a,b)
        renderWithoutCheck(opts,txt,hasFill,hasStroke,hasClip)
        // No need/way to restore, beginPath() is always called
        // when a new path/subpath is started
        break;

      case NODE_TYPE_SEGMENT:
        a   = +HEAPF32[(dr + (1<<2)) >> 2] // x
        b   = +HEAPF32[(dr + (2<<2)) >> 2] // y
        txt =  HEAP32 [(dr + (3<<2)) >> 2]|0 // tail

        _lineTo(a,b)
        // Manual TCo
        dr = txt
        cont = 1
        // renderWithoutCheck(opts,txt,hasFill,hasStroke,hasClip)

        break;
      case NODE_TYPE_SEGMENT2:
        // TODO cubic segment rendering, using quadraticCurveTo
        _debug(ERROR_TYPE_UNKNOWN,0)
        break;

      case NODE_TYPE_SEGMENT3:
        a   = +HEAPF32[(dr + (1<<2)) >> 2] // x1
        b   = +HEAPF32[(dr + (2<<2)) >> 2] // y1
        c   = +HEAPF32[(dr + (3<<2)) >> 2] // x2
        d   = +HEAPF32[(dr + (4<<2)) >> 2] // y2
        e   = +HEAPF32[(dr + (5<<2)) >> 2] // x3
        f   = +HEAPF32[(dr + (6<<2)) >> 2] // y3
        txt =  HEAP32 [(dr + (7<<2)) >> 2]|0 // tail

        _bezierCurveTo(a,b,c,d,e,f)
        // Manual TCo
        dr   = txt
        cont = 1
        // renderWithoutCheck(opts,txt,hasFill,hasStroke,hasClip)
        break;

      case NODE_TYPE_SEGMENT_ARC:
        // TODO arc segment rendering
        _debug(ERROR_TYPE_UNKNOWN,0)
        break;
        break;

      case NODE_TYPE_SEGMENT_END:
        txt =  HEAP32 [(dr + (1<<2)) >> 2]|0 // closed
        if (txt) {
          _closePath()
        }
        if (hasStroke) {
          strokeWithoutPointWiseTransform()
        }
        if (hasFill) {
          _fill()
        }
        break;

      case NODE_TYPE_SEGMENT_SUBPATH:
        txt =  HEAP32 [(dr + (1<<2)) >> 2]|0 // closed
        a   = +HEAPF32[(dr + (2<<2)) >> 2] // x1
        b   = +HEAPF32[(dr + (3<<2)) >> 2] // y1
        dr1 =  HEAP32 [(dr + (4<<2)) >> 2]|0 // tail
        if (txt) {
          _closePath()
        }
        _moveTo(a,b)
        // Manual TCo
        dr   = dr1
        cont = 1
        // renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        break;

      case NODE_TYPE_CLIP:
        dr1 = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr2 = HEAP32[(dr+(2<<2)) >> 2]|0;

        _save()
        renderWithoutCheck(opts,dr2,0,0,1) // No fill or stroke, but clip!
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;


      case NODE_TYPE_FILL_COLOR:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        _save()

        // _fillStyleRGBA(r,g,b,a)
        writeRGBAStringToBuffer(a,b,c,d)
        _fillStyleFromColorBuffer()

        renderWithoutCheck(opts,dr1,1,hasStroke,hasClip) // render with hasFill set
        _restore()
        break;

      case NODE_TYPE_STROKE_COLOR:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        _save()

        // _fillStyleRGBA(r,g,b,a)
        writeRGBAStringToBuffer(a,b,c,d)
        _strokeStyleFromColorBuffer()

        renderWithoutCheck(opts,dr1,hasFill,1,hasClip) // render with hasStroke set
        _restore()
        break;

      case NODE_TYPE_FILL_GRADIENT:
        txt = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        _save()
        _fillGradient(txt|0);
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;

      case NODE_TYPE_FILL_PATTERN:
        txt = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        _save()
        _fillPattern(txt|0);
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;

      case NODE_TYPE_LINE_WIDTH:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        _save()
        _lineWidth(a);
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;

      case NODE_TYPE_LINE_CAP:
        txt = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        _save()
        _lineCap(txt|0);
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;

      case NODE_TYPE_LINE_JOIN:
        txt = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        _save()
        _lineJoin(txt|0);
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;

      case NODE_TYPE_TEXT_FONT:
        txt =  HEAP32 [(dr+(1<<2)) >> 2]|0 // txt
        dr1 =  HEAP32 [(dr+(2<<2)) >> 2]|0 // txt
        _save()
        _font(txt|0)
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;

      case NODE_TYPE_TEXT_ALIGNMENT:
        txt =  HEAP32 [(dr+(1<<2)) >> 2]|0 // val
        dr1 =  HEAP32 [(dr+(2<<2)) >> 2]|0 // txt
        _save()
        _textAlign(txt|0)
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;

      case NODE_TYPE_TEXT_BASELINE:
        txt =  HEAP32 [(dr+(1<<2)) >> 2]|0 // val
        dr1 =  HEAP32 [(dr+(2<<2)) >> 2]|0 // txt
        _save()
        _textBaseline(txt|0)
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;


      case NODE_TYPE_TAG:
        txt =  HEAP32 [(dr+(1<<2)) >> 2]|0 // tag name
        dr1 =  HEAP32 [(dr+(2<<2)) >> 2]|0 // drawing
        // Ignore tags
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        break;

      case NODE_TYPE_TRANSF:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        e = +HEAPF32[(dr+(5<<2)) >> 2];
        f = +HEAPF32[(dr+(6<<2)) >> 2];
        dr1 = HEAP32[(dr+(7<<2)) >> 2]|0;

        // Could be optimized with selective version of save/restore
        // Or simply apply inverted matrix when done http://stackoverflow.com/a/18504573
        // Or use currentTransform if supported
        _save()
        _transform(a,b,c,d,e,f)
        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)
        _restore()
        break;

      case NODE_TYPE_AP2:
        dr1 = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr2 = HEAP32[(dr+(2<<2)) >> 2]|0;

        renderWithoutCheck(opts,dr1,hasFill,hasStroke,hasClip)

        // render(opts,dr2)
        // Manual tail-call opt: Instead of calling this function again, we update the parameters and set cont to true
        opts = opts
        dr   = dr2
        cont = 1

        break;

      default:
        _debug(ERROR_TYPE_UNKNOWN, drType|0);
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
    renderWithoutCheck(opts,dr,0,0,0)
  }

  // Return the tag of the top-most part of the drawing at the given point or
  //
  //  ERROR_NO_TAG if the point is in a path that has no tag
  //  ERROR_POINT_OUTSIDE is the point is outside the path of the given drawing.
  //
  // Opts* -> Drawing* -> P2 -> ( ERROR_NO_TAG | ERROR_POINT_OUTSIDE | Int)
  function getPointTag_(opts,dr,x,y) {
    opts = opts|0;
    dr = dr|0;
    x = +x;
    y = +y;
    return getPointTag(opts,dr,x,y,ERROR_NO_TAG)|0;
  }

  function getPointTag(opts,dr,x,y,tag) {
    opts = opts|0;
    dr = dr|0;
    x = +x;
    y = +y;
    tag = tag|0

    // opts = opts|0;
    // dr = dr|0;
    // hasFill = hasFill|0;
    // hasStroke = hasStroke|0;
    // hasClip = hasClip|0 // TODO not actually used

    /* Don't rely on these default values as manual TCO (see below) may caused
      them to have a different value. */
    var drType = 0

    var a = 0.
    var b = 0.
    var c = 0.
    var d = 0.
    var e = 0.
    var f = 0.

    var dr1 = 0
    var dr2 = 0
    var txt = 0 // Bad name

    var cont = 0

    /*
    Manual TCO, see comments in rendering functions above.
    */
    do {
    cont = 0
    drType = getPtrType(dr)|0;

    switch (drType|0) {

      case NODE_TYPE_FREE:
        _debug(ERROR_TYPE_UNKNOWN, 0);
        break;

      case NODE_TYPE_CIRCLE:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        // drawCircle(a,b,c,hasFill,hasStroke,hasClip)
        // TODO
        return ERROR_POINT_OUTSIDE

      case NODE_TYPE_RECT:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        // drawRect(a,b,c,d,hasFill,hasStroke,hasClip)
        // TODO
        return ERROR_POINT_OUTSIDE

      case NODE_TYPE_TEXT:
        a   = +HEAPF32[(dr + (1<<2)) >> 2]
        b   = +HEAPF32[(dr + (2<<2)) >> 2]
        txt =  HEAP32 [(dr + (3<<2)) >> 2]|0 // txt

        // We don't support text for clipping paths
        return ERROR_POINT_OUTSIDE

      case NODE_TYPE_PATH:
        a   = +HEAPF32[(dr + (1<<2)) >> 2] // x
        b   = +HEAPF32[(dr + (2<<2)) >> 2] // y
        txt =  HEAP32 [(dr + (3<<2)) >> 2]|0 // segments

        _beginPath()
        _moveTo(a,b)
        return getPointTag(opts,txt,x,y,tag)|0

      case NODE_TYPE_SEGMENT:
        a   = +HEAPF32[(dr + (1<<2)) >> 2] // x
        b   = +HEAPF32[(dr + (2<<2)) >> 2] // y
        txt =  HEAP32 [(dr + (3<<2)) >> 2]|0 // tail

        _lineTo(a,b)
        // Manual TCO
        // dr = txt
        // cont = 1
        return getPointTag(opts,txt,x,y,tag)|0

      // case NODE_TYPE_SEGMENT2:
      //   // TODO cubic segment detection with quadraticCurveTo
      //   _debug(ERROR_TYPE_UNKNOWN,0)
      //   break;
      //
      // case NODE_TYPE_SEGMENT3:
      //   a   = +HEAPF32[(dr + (1<<2)) >> 2] // x1
      //   b   = +HEAPF32[(dr + (2<<2)) >> 2] // y1
      //   c   = +HEAPF32[(dr + (3<<2)) >> 2] // x2
      //   d   = +HEAPF32[(dr + (4<<2)) >> 2] // y2
      //   e   = +HEAPF32[(dr + (5<<2)) >> 2] // x3
      //   f   = +HEAPF32[(dr + (6<<2)) >> 2] // y3
      //   txt =  HEAP32 [(dr + (7<<2)) >> 2]|0 // tail
      //
      //   _bezierCurveTo(a,b,c,d,e,f)
      //   // Manual TCO
      //   // dr   = txt
      //   // cont = 1
      //   return getPointTag(opts,txt,x,y,tag)|0

      // case NODE_TYPE_SEGMENT_ARC:
        // TODO arc segment detection
        // _debug(ERROR_TYPE_UNKNOWN,0)
        // break;

      case NODE_TYPE_SEGMENT_END:
        txt =  HEAP32 [(dr + (1<<2)) >> 2]|0 // closed
        if (txt) {
          _closePath()
        }
        // console.log("FIXME testing", x, y)
        // _printCurrentPath()
        // console.log("  Current tag", tag)
        if (_isPointInPath(x, y)|0) {
          // console.log("  -----> Yes", x, y)
          return tag|0
        } else {
          // console.log("  No", x, y)
          return ERROR_POINT_OUTSIDE
        }

      case NODE_TYPE_SEGMENT_SUBPATH:
        txt =  HEAP32 [(dr + (1<<2)) >> 2]|0 // closed
        a   = +HEAPF32[(dr + (2<<2)) >> 2] // x1
        b   = +HEAPF32[(dr + (3<<2)) >> 2] // y1
        dr1 =  HEAP32 [(dr + (4<<2)) >> 2]|0 // tail
        if (txt) {
          _closePath()
        }
        _moveTo(a,b)
        // Manual TCO
        // dr   = dr1
        // cont = 1
        return getPointTag(opts,dr1,x,y,tag)|0

      case NODE_TYPE_TAG:
        txt =  HEAP32 [(dr+(1<<2)) >> 2]|0 // tag name
        dr1 =  HEAP32 [(dr+(2<<2)) >> 2]|0 // drawing
        return getPointTag(opts,dr1,x,y,txt)|0 // Use the declared tag ("txt")

      case NODE_TYPE_TRANSF:
        a = +HEAPF32[(dr+(1<<2)) >> 2];
        b = +HEAPF32[(dr+(2<<2)) >> 2];
        c = +HEAPF32[(dr+(3<<2)) >> 2];
        d = +HEAPF32[(dr+(4<<2)) >> 2];
        e = +HEAPF32[(dr+(5<<2)) >> 2];
        f = +HEAPF32[(dr+(6<<2)) >> 2];
        dr1 = HEAP32[(dr+(7<<2)) >> 2]|0;

        // Could be optimized with selective version of save/restore
        // Or simply apply inverted matrix when done http://stackoverflow.com/a/18504573
        // Or use currentTransform if supported
        _save()
        _transform(a,b,c,d,e,f)
        txt = getPointTag(opts,dr1,x,y,tag)|0
        _restore()
        return txt|0

      case NODE_TYPE_AP2:
        dr1 = HEAP32[(dr+(1<<2)) >> 2]|0;
        dr2 = HEAP32[(dr+(2<<2)) >> 2]|0;
        // Use txt for results

        txt = getPointTag(opts,dr1,x,y,tag)|0
        if ((txt|0) != ERROR_POINT_OUTSIDE) {
          return txt|0
        } else {
          return getPointTag(opts,dr2,x,y,tag)|0
        }

      case NODE_TYPE_CLIP:
        _debug(ERROR_TYPE_UNKNOWN, drType|0);
        break;

      case NODE_TYPE_FILL_COLOR:
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0
      case NODE_TYPE_STROKE_COLOR:
        dr1 = HEAP32[(dr+(5<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0
      case NODE_TYPE_FILL_GRADIENT:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0
      case NODE_TYPE_FILL_PATTERN:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0
      case NODE_TYPE_LINE_WIDTH:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0
      case NODE_TYPE_LINE_CAP:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0
      case NODE_TYPE_LINE_JOIN:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0
      case NODE_TYPE_TEXT_FONT:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0
      case NODE_TYPE_TEXT_ALIGNMENT:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0
      case NODE_TYPE_TEXT_BASELINE:
        dr1 = HEAP32[(dr+(2<<2)) >> 2]|0;
        return getPointTag(opts,dr1,x,y,tag)|0

      default:
        _debug(ERROR_TYPE_UNKNOWN, drType|0);
        break;
    }
    }
    while(cont);

    return ERROR_NO_TAG;
  }

  // etc
  return  { render : render
          , getPointTag_ : getPointTag_ // TODO fix names

          , primCircle : primCircle
          , primRect : primRect
          , primText : primText
          , primPath : primPath
          , primSegment : primSegment
          , primSegment2 : primSegment2
          , primSegment3 : primSegment3
          , primSegmentArc : primSegmentArc
          , primSegmentEnd : primSegmentEnd
          , primSegmentSubpath : primSegmentSubpath

          , primTransf : primTransf

          , primFillColor : primFillColor
          , primFillGradient : primFillGradient
          , primFillPattern : primFillPattern
          , primStrokeColor : primStrokeColor
          , primLineWidth : primLineWidth
          , primLineCap : primLineCap
          , primLineJoin : primLineJoin
          , primTextFont : primTextFont
          , primTextAlignment : primTextAlignment
          , primTextBaseline : primTextBaseline

          , primTag : primTag

          , primAp2 : primAp2
          , primClip : primClip

          , claim : claim
          , release : release

          , getMaxNumberOfTuples : getMaxNumberOfTuples
          , getCurrentTuples : getCurrentTuples
          , slotIndexToPtr : slotIndexToPtr
          , getPtrType : getPtrType
          , addToRefCount : addToRefCount
          , getRefCount : getRefCount
          , newTuple : newTuple
          // , allocateTupleInitPhase : allocateTupleInitPhase
          , allocateTupleScanning : allocateTupleScanning
          , getAllocationInfo : getAllocationInfo
          }
}

function createRenderer(c2) {
  const c = c2

  var heap        = new ArrayBuffer(HEAP_SIZE)

  var colorBuffer = new Uint8Array(heap, HEAP_COLOR_BUFFER_OFFSET, 22);
  var utf8d       = new TextDecoder("utf-8");

  // Externals
  // FIXME size of this table leaks
  var externals   = {}
  var externalCount = 0

  // Passing arguments (strins/images) to renderer will store object here and transfer
  // ownership to renderer node. When the node is deallocated, object disappears from here.
  // object|string -> Int
  function storeExternal(obj) {
    var n = externalCount++
    externals[n] = obj
    return n
  }
  // Int -> object|string|undefined
  function fetchExternal(n) {
    return externals[n]
  }
  // Int -> undefined (called by asm)
  function releaseExternal(n) {
    externals[n] = undefined
  }

  var currentPath = ""
  // ASM module is linked here...
  var res = new AsmDrawingRenderer(window,
      { random : Math.random
      , releaseExternal : releaseExternal

      , isPointInPath: function(x, y) { if (c.isPointInPath(x,y))
          {
              // console.log("YES");
              return 1 }
          else
          {
              // console.log("NO");
              return 0 } }

      , beginPath: function () { c.beginPath() }
      , moveTo: function (x,y) { c.moveTo(x,y) }
      , closePath: function () { c.closePath() }
      , lineTo: function (x,y) { c.lineTo(x,y) }
      // , beginPath: function () { currentPath = ""; c.beginPath() }
      // , moveTo: function (x,y) { currentPath += " M" + x + "," + y; c.moveTo(x,y) }
      // , closePath: function () { currentPath += " C"; c.closePath() }
      // , lineTo: function (x,y) { currentPath += " L" + x + "," + y; c.lineTo(x,y) }
      , printCurrentPath: function () { console.log(currentPath) }

      , quadraticCurveTo: function (cpx, cpy, x, y) { c.quadraticCurveTo(cpx, cpy, x, y) }
      , bezierCurveTo: function (cp1x, cp1y, cp2x, cp2y, x, y) { c.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y) }
      , arcTo: function (x1, y1, x2, y2, radius) { c.arcTo(x1, y1, x2, y2, radius) }
      , arc: function (x, y, radius, startAngle, endAngle, anticlockwise) { c.arc(x, y, radius, startAngle, endAngle, anticlockwise) }
      , rect: function (x, y, w, h) { c.rect(x, y, w, h) }

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
      , fillGradient:
      function (x) {
        c.fillGradient = fetchExternal(ref)
      }
      , fillPattern:
      function (x) {
        c.fillPattern = fetchExternal(ref)
      }
      , lineWidth:
      function (x) {
        c.lineWidth = x
      }
      , lineCap:
      function (x) {
        switch (x) {
          case STYLE_LINE_CAP_BUTT:
            c.textAlign = "butt"
            break
          case STYLE_LINE_CAP_ROUND:
            c.textAlign = "round"
            break
          case STYLE_LINE_CAP_SQUARE:
            c.textAlign = "square"
            break
        }
      }
      , lineJoin:
      function (x) {
        switch (x) {
          case STYLE_LINE_JOIN_BEVEL:
            c.textAlign = "bevel"
            break
          case STYLE_LINE_JOIN_ROUND:
            c.textAlign = "round"
            break
          case STYLE_LINE_JOIN_MITER:
            c.textAlign = "miter"
            break
        }
      }
      , font:
      function (ref) {
        c.font = fetchExternal(ref)
      }
      , textAlign:
      function (x) {
        switch (x) {
          case STYLE_TEXT_ALIGN_END:
            c.textAlign = "end"
            break
          case STYLE_TEXT_ALIGN_LEFT:
            c.textAlign = "left"
            break
          case STYLE_TEXT_ALIGN_RIGHT:
            c.textAlign = "right"
            break
          case STYLE_TEXT_ALIGN_START:
            c.textAlign = "start"
            break
          case STYLE_TEXT_ALIGN_CENTER:
            c.textAlign = "center"
            break
        }
      }
      , textBaseline:
      function (x) {
        switch (x) {
          case STYLE_TEXT_BASELINE_TOP:
            c.textBaseline = "top"
            break
          case STYLE_TEXT_BASELINE_BOTTOM:
            c.textBaseline = "bottom"
            break
          case STYLE_TEXT_BASELINE_MIDDLE:
            c.textBaseline = "middle"
            break
          case STYLE_TEXT_BASELINE_HANGING:
            c.textBaseline = "hanging"
            break
          case STYLE_TEXT_BASELINE_ALPHABETIC:
            c.textBaseline = "alphabetic"
            break
          case STYLE_TEXT_BASELINE_IDEOGRAPHIC:
            c.textBaseline = "ideographic"
            break
        }
      }
      , fillStyleRGBA:
        function (r,g,b,a) {
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
      , fillRect:
        function (x,y,w,h) {
          x = +x
          y = +y
          w = +w
          h = +h
          c.fillRect(x,y,w,h)
        }
      , strokeRect:
        function (x,y,w,h) {
          x = +x
          y = +y
          w = +w
          h = +h
          c.strokeRect(x,y,w,h)
        }
      , fillText:
        function (x,y,txtRef) {
          c.fillText(fetchExternal(txtRef), x, y)
        }
      , setTransform:
        function (a, b, c_, d, e, f) { c.setTransform(a,b,c_,d,e,f) }
      , save:
        function (x) { c.save() }
      , restore:
        function (x) { c.restore() }
      , transform:
        function (a,b,c_,d,e,f) {
          a = +a
          b = +b
          c_ = +c_
          d = +d
          e = +e
          f = +f

          c.transform(a,b,c_,d,e,f)
        }
      , debug:
        function (msg, arg) {
          switch (msg) {
            case ERROR_TYPE_UNKNOWN:
              console.log("Error: ", "Unknown node type", arg)
              break;
            case ERROR_TYPE_UNKNOWN_RELEASE:
              console.log("Error: ", "Unknown node type (release)")
              break;
            case ERROR_OUT_OF_MEMORY:
              console.log("Error: ", "Out of memory")
              break;
            case ERROR_TYPE_FREE_PASSED_TO_RENDER:
              console.log("Error: ", "Strange value passed to render, probably caused by a bad deallocation")
              break;
            default:
              console.log("Error: ", "(unknown)")
              break;
          }
         }
      }, heap)

  // Store a reference to the context (mainly for debugging)
  res.context = c

  // Wrappers / Helpers
  res.rect = res.primRect

  res.text = function(x, y, text) {
    var textRef = storeExternal(text)
    return res.primText(x, y, textRef)
  }
  res.textFont = function(font, dr) {
    var fontRef = storeExternal(font)
    return res.primTextFont(fontRef, dr)
  }
  res.dumpExternals = function() {
    Object.keys(externals).forEach(k => console.log("  ", k, fetchExternal(k)))
  }

  res.ap = function(xs) {
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

  res.dumpHeap = function () {
    res.dumpHeapUsage()
    console.log("Dumping heap below...")
    console.log("")
    console.log("Color buffer:", utf8d.decode(colorBuffer))
    res.dumpTuples()
  }
  res.dumpHeapUsage = function () {
    var n = res.getMaxNumberOfTuples()
    var unusedTuples = 0;
    for (var i = 0; i < n; ++i) {
      var ptr = res.slotIndexToPtr(i)
      if (res.getPtrType(ptr) === NODE_TYPE_FREE) {
        ++unusedTuples;
      }
    }
    var usedTuples = n - unusedTuples;
    var allocs = res.getAllocationInfo(1)
    var deallocs = res.getAllocationInfo(0)
    console.log("Heap size:", HEAP_SIZE)
    console.log("Max tuples:", res.getMaxNumberOfTuples())
    console.log("Number of tuples allocated:", allocs)
    console.log("Number of tuples deallocated:", deallocs)

    // Difference between allocs/deallocs vs current number of tuples on
    // the heap (as per scaning types). This should be zero or there is a bug
    // in the allocator.
    console.log("Lost tuples:", allocs - (deallocs + usedTuples))

    console.log("Current number of tuples:", usedTuples)
    console.log("Current heap usage", Math.floor(usedTuples*100/n), "%")
  }

  res.dumpTuple = function (ptr, includeSlots) {
    console.log("Tuple at address", ptr)
    console.log("  ", "Type:", res.getPtrType(ptr))
    console.log("  ", "(External) references:", res.getRefCount(ptr))
    if (includeSlots) {
      // Actually tricky, as we have no type tags
    }
  }

  res.dumpTuples = function () {
    var n = res.getMaxNumberOfTuples()
    var maxDump = 1000
    if (n > maxDump) {
      console.log("Error: Refusing to dump more than ", maxDump, " tuples")
    } else {
      for (var i = 0; i < n; ++i) {
        var ptr = res.slotIndexToPtr(i)
        res.dumpTuple(ptr, false)
      }
    }
  }

  // TODO debug
  window.globalRenderer = res
  window.g = res
  return res
}
