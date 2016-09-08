
var dims = {x:800, y:800}
var elem = document.getElementById('canvas-div');
var nElems = 5000
var nMoving = 100

//
// //------------------------------
// var two = new Two({
//   type: Two.Types.webgl,
//   width: dims.x,
//   height: dims.y
// }).appendTo(elem);
//
// var group = two.makeGroup();
// var circles = [...Array(nElems).keys()].map(function(i) {
//   var c = two.makeCircle(Math.floor(Math.random() * dims.x), Math.floor(Math.random() * dims.y), 4);
//   // c.fill = ['red', 'green', 'blue'][Math.floor(Math.random() * 3)]
//   c.fill = 'grey'
//   c.noStroke()
//   group.add(c)
//   return c
// })
// circles.reverse() // Side-effects!
// console.log("reversed")
// two.update();
// // Bind a function to scale and rotate the group
// // to the animation loop.
// two.bind('update', function(frameCount) {
//    if ((frameCount % 1) !== 0) return;
//    for (var i = 0; i < Math.min(circles.length, nMoving); ++i) {
//      var c = circles[i]
//      c.fill = 'red'
//      c.translation.y = (c.translation.y + 1) % dims.x
//    }
// }).play(); // Finally, start the animation loop
// //------------------------------




// State
var canvas = document.getElementById('canvas');
var c = canvas.getContext('2d');
// console.log(c)

function drawCircle(c,x,y,r) {
  c.beginPath();
  c.arc(x,y,r, 0, 2 * Math.PI, false);
  c.fill();
}


function renderStyle(name, value, context) {
  switch (name) {
    case 'fillColor':
      context.fillStyle = value; // TODO opacity etc
      break;
    default:
      throw ('renderStyle: Unknown style ' + name);
  }
}
/*
Render the given drawing object using the given context.
Does NOT clear the rectangle beforehand.
FIXME adjust co-ordinates based on opts.originPlacement, opts.dimensions (a la Drawing)
*/
function renderDrawing(opts, drawing, context) {
  if (opts === undefined) opts = {};

  switch (drawing.type) {
    case 'circle':
      drawCircle(context, drawing.x, drawing.y, drawing.rad);
      break;
    // case 'rect':
    //   break;
    case 'transf':
      context.save()
      let [a,b,c,d,e,f] = drawing.transf;
      context.transform(a,b,c,d,e,f)
      renderDrawing(opts, drawing.sub, context)
      // drawing.children.forEach (sd => renderDrawing(opts, sd, context))
      context.restore()
      break;
    case 'style':
      context.save()
      renderStyle(drawing.name, drawing.value, context)
      renderDrawing(opts, drawing.sub, context)
      // drawing.children.forEach (sd => renderDrawing(opts, sd, context))
      context.restore()
      break;
    case 'ap':
      drawing.children.forEach (sd => renderDrawing(opts, sd, context))
      break;
    default:
      throw ('renderDrawing: Unknown drawing type ' + drawing.type);
  }

}

function replicate(n,x) {
  return [...Array(n).keys()].map(ignored => x)
}
const drawingDef = {type:'transf', transf:[2,0,0,1,0,0], sub:
  {type:'style', name:'fillColor', value:'green', sub:
    {type:'ap', children:
      replicate(nElems, {type: 'circle', x:10, y:10, rad:10}
      )
      }
  }
};

function setup () {
  // c.fillCircle(50,50,50,50)
  // drawCircle(c, dims.x/2, dims.x/2, 5)


}
function drawFrame() {
  c.clearRect(0, 0, dims.x, dims.y);
  renderDrawing({},
    drawingDef
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
