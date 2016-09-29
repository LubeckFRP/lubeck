
var dims = {x:1900, y:1500}
var nElems  = 500
var nMaxFrames = 60*10

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
  fastContext.fillStyle   = "rgba(0,0,0,0)"
  fastContext.strokeStyle = "rgba(0,0,0,0)"
  // WebGL2D.enable(canvas);
  // fastContext = canvas.getContext('webgl-2d');


  fastRenderer = createRenderer(fastContext)
  fastRenderer.randPosRect = function() {
    return fastRenderer.primRect(Math.floor(Math.random()*dims.x),Math.floor(Math.random()*dims.y),30,30)
  }
  fastRenderer.randPosCircle = function() {
    return fastRenderer.primCircle(Math.floor(Math.random()*dims.x),Math.floor(Math.random()*dims.y),30)
  }

// > writeFile "/tmp/lubeck/test1.svg" $ unpackStr $ toSvgStr mempty
// $ fillColor C.blue $ mconcat [translateX 200 $ scale 100 circle, fillColor C.red $ scale 200 circle]

  r = fastRenderer
  fastDrawing =
    r.ap(
      [ r.empty()
      , r.ap(replicateM(Math.floor(nElems/2),_ =>
          r.blue(r.ap(
              [ r.empty()
              , r.primLineWidth(10, r.primStrokeColor(0,0,0,0, r.red(r.scale(0.5*(0.8+0.2*Math.random()),r.randPosRect()))))
              // , r.randCol(r.scale(0.8+0.2*Math.random(),r.randPosRect()))
            ]))))
      , r.ap(replicateM(Math.floor(nElems/2),_ =>
          r.blue(r.ap(
              [ r.empty()
              // , r.primLineWidth(10, r.primStrokeColor(0,0,0,1, r.red(r.scale(2*(0.8+0.2*Math.random()),r.randPosCircle()))))
              , r.randCol(r.scale(0.8+0.2*Math.random(),r.randPosRect()))
            ]))))

    ])
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
