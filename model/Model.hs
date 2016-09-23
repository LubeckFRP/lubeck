
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


data Segment
  = Segment P2 Segment -- line
  | Segment2 P2 P2 Segment -- quadratic bezier
  | Segment3 P2 P2 P2 Segment-- cubic bezier (7 slots!)
  | Arc P2 P2 Double Segment -- 2 control points, radius
  | End Bool -- true to close the segment

data Drawing
  -- These are
  -- structurally collections of points
  -- semantially (Env -> 2DPos -> AlphaColor)
  = Free -- Used for GC etc, never rendered
  | Circle P2 Double
  | Rect P2 Double Double
  | Text P2 TextRef -- TODO
  -- All above is just an optimized form of Path...
  | Path P2 Segment -- TODO

  -- semantically (2DPos -> 2DPos)
  -- Transforms all points/shapes in the nested drawing
  | Transf Matrix Drawing

  -- semantically (Env -> 2DPos -> AlphaColor)
  -- the non-drawing part is semantically (Env -> Env)
  | FillColor RGBA Drawing   -- Causes paths in the nested drawing to be filled with that color (global env has transparent)
  | StrokeColor RGBA Drawing -- Causes paths in the nested drawing to be stroked with that color (global env has transparent)
    -- affects stroking
    | LineWidth Double Drawing
    | LineCap LineCap Drawing
    | LineJoin lineJoin Drawing
    -- | LineDash LineDash Drawing -- TODO
  -- | FillGradient (GradientRef) -- TODO
  -- | FillPattern (GradientRef) -- TODO
    | TextFont TextRef Drawing
    | TextAlign TextAlign Drawing
    | TextBaseline TextBaseline Drawing

  -- Tag the "filled" part of the drawing
  -- This can be seen as defining an arbitrary number of binary masks (in addition to standard stroke/fill)
  | Tag Tag Drawing

  -- Put arg2 on top of arg1
  -- or equivalently: "draw arg1, then draw arg2"
  | Ap2 Drawing Drawing
  -- Any shapes in arg2 is used to clip arg1
  -- or equivalently: "draw arg1, then clip it using arg2"
  -- We don't support
  | Clip2 Drawing Drawing
