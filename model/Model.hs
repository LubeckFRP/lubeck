
-- Layout of the renderer heap (tuple part)

-- Each constructor below maps to an asm tuple with 7 slots
-- "Unboxed tuples" occupy the expected number of slots (i.e. 4 for RGBA)
-- The maximum number of slots if 7 (excluding slot 0 which is used by GC and for the type tag)
-- Slot types are pointers, Int, Double, and "unboxed" things as per above

-- These things are
type RGBA = (# Double,Double,Double,Double #)
type Matrix = (# Double,Double,Double,Double,Double,Double #)
type Tag = #Int
data LineCap = ... -- enum
data LineDash = ... -- enum

data Drawing
  -- These are
  -- structurally collections of points
  -- semantially (Env -> 2DPos -> AlphaColor)
  = Circle Double Double Double
  | Rect Double Double Double Double
  | Text -- TODO
  -- All above is just an optimized form of Path...
  | Path -- TODO

  -- semantically (Env -> 2DPos -> AlphaColor)
  -- the non-drawing part is semantically (Env -> Env)
  | FillColor RGBA Drawing
  | StrokeColor RGBA Drawing
  -- Semantically
  -- This could be thought of as generating a new mask for each tag (in addition to the fill/stroke/clip masks)
  | Tag Tag Drawing

  | LineWidth Double Drawing
  | LineCap LineCap Drawing
  | LineJoin lineJoin Drawing
  | LineDash LineDash Drawing -- TODO

  -- semantically (2DPos -> 2DPos)
  | Transf Matrix Drawing

  -- A specific rasterized dataset (could be viewed as an optimization for lots of small rects)
  | Image -- TODO



  -- Put arg2 on top of arg1
  -- or equivalently: "draw arg1, then draw arg2"
  | Ap2 Drawing Drawing
  -- Any shapes in arg2 is used to clip arg1
  -- or equivalently: "draw arg1, then clip it using arg2"
  -- We don't support
  | Clip2 Drawing Drawing
