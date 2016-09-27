
-- Layout of the renderer heap (tuple part)

-- Each constructor below maps to an asm tuple with 7 slots
-- "Unboxed tuples" occupy the expected number of slots (i.e. 4 for RGBA)
-- The maximum number of slots if 7 (excluding slot 0 which is used by GC and for the type tag)
-- Slot types are pointers, Int, Double, and "unboxed" things as per above

-- These things are
type RGBA = (# Double,Double,Double,Double #) -- 4
type Matrix = (# Double,Double,Double,Double,Double,Double #) -- 6
type Tag = #Int
type P2 = (# Double, Double #)
type V2 = (# Double, Double #)
data LineCap = ... -- enum
data LineDash = ... -- enum
type TextRef = Int -- stored externally
type ImageRef = Int -- stored externally

{-
(from Canvas Spec)

A path has a list of zero or more subpaths. Each subpath consists of a list
of one or more points, connected by straight or curved lines, and a flag
indicating whether the subpath is closed or not. A closed subpath is one
where the last point of the subpath is connected to the first point of the
subpath by a straight line.

Subpaths with fewer than two points are ignored when painting the path.
-}
data TextAlign = TextAlignStart | TextAlignEnd | TextAlignLeft | TextAlignRight | TextAlignCenter -- TODO
  deriving (Eq, Ord, Enum, Show)
data TextBaseline
  = TextBaselineTop | TextBaselineHanging | TextBaselineMiddle
  | TextBaselineAlphabetic | TextBaselineIdeographic | TextBaselineBottom -- TODO
  deriving (Eq, Ord, Enum, Show)
data LineCap = LineCapButt | LineCapRound | LineCapSquare
  deriving (Eq, Ord, Enum, Show)
data LineJoin = LineJoinBevel | LineJoinRound | LineJoinMiter
  deriving (Eq, Ord, Enum, Show)
data FillRule = FillRuleNonZero | FillRuleEvenOdd
  deriving (Eq, Ord, Enum, Show)

data Segments
  = Segment !P2 Segments -- line

  -- TODO optimize as follows
  -- | SegmentX2 !P2 !P2 Segments -- SegmentX2 p1 p2 a == Segment p1 (Segment p2 a)
  -- | SegmentX3 !P2 !P2 !P2 Segments -- SegmentX2 p1 p2 p3 a == Segment p1 (Segment p2 (Segment p3 a))

  -- lineTo
  | Segment2 !P2 !P2 Segments -- quadratic bezier
  -- quadraticCurveTo
  | Segment3 !P2 !P2 !P2 Segments-- cubic bezier (7 slots!)
  --  bezierCurveTo
  | SegmentArc !P2 !P2 !Double Segments -- 2 control points, radius
  -- arcTo
  | SegmentEnd !Bool -- true to close the segment
  --  optional closePath, then moveTo

  -- TODO is this strictly necessary?
  | SegmentSubpath !Bool P2 Segments -- true to close the segment, then move and start new segment
  --  optional closePath, then moveTo

-- data Paths
  -- = Paths1 P2 Segments
  -- | Paths1 ()

{-
TODO understand paths vs event detection and clip

Event detection will be a function that walks down the tree with a point, applies inverse transformations to the
point and runs event detection on the paths it encounter using the transformed point. No need to transform the
path as long as the point as been appropriatly transformed. For primitives (circle, rect) we can write our ownership
detection or fake a path, for text event detection is not supported.


TODO Segment vs Clip


NOTE Drawing API consists of mutation of
  - Current path
  - Drawing state (transformation, clipping region, stroke/fill, misc styles such as line/text etc)
  - Basic operation is to
      Modify drawing state
      Push path components (segments, closing or jumps)
      Render (stroke/fill)

  We represent a drawing as a tree that is traversed depth first, where leaves are path components and branches
  modify the drawing state. Rendering happens automatically based on environment, so any leave that
  has a stroke/fill style node above it will be stroked/filled.

  For clipping we use a special clipping subtree that supports leaves (paths) and branches (intersection).
  Note that we can use path concatenation to represent union of clipping regions.
  TODO likely performance/simplicity trade-offs

  NOTE Path detection
    As per here
      https://bugzilla.mozilla.org/show_bug.cgi?id=405300
    The isPointInPath() funtion actually behaves sensibly

    It transform the checked point using current transformation (CT) and then checks
    that it is inside the current path (presumably created using the same CT).

    Instead of path-based detection we will have a function that returns the tag of
    the top-most part of the image.
      getTag :: P2 -> Renderer -> Drawing -> IO (Maybe Tag)

    Implementation simply traverses drawing tree, applies transformations and calls
    isPointInPath(), returning the current tag on the first hit.

    Because this traversal is not rendering, hasStroke/hasFill will be set to false.
-}


data Drawing
  -- These are
  -- structurally collections of points
  -- semantially (Env -> 2DPos -> AlphaColor)
  = Free -- Used for GC etc, never rendered
  | Circle !P2 !Double
  | Rect !P2 !Double !Double
  | Text !P2 TextRef
  -- All above is just an optimized form of Path...
  | Path !P2 Segment -- TODO
  -- beginPath, moveTo

  -- semantically (2DPos -> 2DPos)
  -- Transforms all points/shapes in the nested drawing
  | Transf !Matrix Drawing

  -- semantically (Env -> 2DPos -> AlphaColor)
  -- the non-drawing part is semantically (Env -> Env)
  | FillColor! RGBA Drawing   -- Causes paths in the nested drawing to be filled with that color (global env has transparent)
  | FillGradient GradientRef Drawing
  | FillPattern PatternRef Drawing
  | StrokeColor !RGBA Drawing -- Causes paths in the nested drawing to be stroked with that color (global env has transparent)
    -- affects stroking
  | FillRule !FillRule -- TODO set fill rule for the nested drawing
    | LineWidth !Double Drawing
    | LineCap !LineCap Drawing
    | LineJoin !LineJoin Drawing
    | TextFont TextRef Drawing
    | TextAlign !TextAlign Drawing
    | TextBaseline !TextBaseline Drawing

  -- Tag the "filled" part of the drawing
  -- This can be seen as defining an arbitrary number of binary masks (in addition to standard stroke/fill)
  | Tag Tag Drawing -- TODO

  -- Put arg2 on top of arg1
  -- or equivalently: "draw arg1, then draw arg2"
  | Ap2 Drawing Drawing
  -- Any shapes in arg2 is used to clip arg1
  -- or equivalently: "draw arg1, then clip it using arg2"
  -- We don't support
  | Clip Drawing P2 Segments -- TODO
