
var dims = {x:800, y:800}
var elem = document.getElementById('canvas');
var two = new Two({
  type: Two.Types.webgl,
  width: dims.x,
  height: dims.y
}).appendTo(elem);


var group = two.makeGroup();
var circles = [...Array(200).keys()].map(function(i) {
  var c = two.makeCircle(Math.floor(Math.random() * dims.x), Math.floor(Math.random() * dims.y), 4);
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
   if ((frameCount % 2) !== 0) return;

  //  circles.forEach(c => c.translation.y = (c.translation.y + 1) % dims.x)
  //  circles.forEach(c => c.translation.x = (c.translation.x + 1)  % dims.y)

   for (var i = 0; i < Math.min(circles.length, 50); ++i) {
     var c = circles[i]
     c.fill = 'red'
     c.translation.y = (c.translation.y + 1) % dims.x
    //  c.translation.x = (c.translation.x + 1) % dims.y
   }

    //  group.translation.y = (group.translation.y + 1) % dims.x
    //  group.translation.x = (group.translation.x + 1)  % dims.y



  //  for (var i = 0; i < 400; ++i) {
  //		var c = circles[i];
  //    c.translation.x = (c.translation.x + 1) % 400;
  //  }
}).play(); // Finally, start the animation loop
