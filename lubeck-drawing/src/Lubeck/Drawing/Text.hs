
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor, TypeFamilies, OverloadedStrings,
  NamedFieldPuns, CPP, NoMonomorphismRestriction, BangPatterns, StandaloneDeriving
  , ScopedTypeVariables #-}

module Lubeck.Drawing.Text
  ( TextAnchor(..)
  , AlignmentBaseline(..)
  , FontStyle(..)
  , FontWeight(..)
  , FontSize(..)
  , TextOptions(..)
  , textOptionsToStyle
  )
where

import BasePrelude

import Lubeck.Str
import Lubeck.Drawing.Style


-- | Text anchor (horizontal alignment).
--
-- https://www.w3.org/TR/SVG/text.html#AlignmentProperties
data TextAnchor
  = TextAnchorStart
  | TextAnchorMiddle
  | TextAnchorEnd
  | TextAnchorInherit
  deriving (Eq, Ord, Read, Show)

instance Monoid TextAnchor where
  mappend x y
    | y == mempty = x
    | otherwise   = y
  mempty = TextAnchorInherit

-- | Text baseline (vertical) alignment.
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/alignment-baseline
-- https://www.w3.org/TR/SVG/text.html#AlignmentProperties
--
-- TODO rename this type DominantBaseline (that's what it actually generates!)
data AlignmentBaseline
  = AlignmentBaselineAuto
  | AlignmentBaselineBaseline
  | AlignmentBaselineBeforeEdge
  | AlignmentBaselineTextBeforeEdge
  | AlignmentBaselineMiddle
  | AlignmentBaselineCentral
  | AlignmentBaselineAfterEdge
  | AlignmentBaselineTextAfterEdge
  | AlignmentBaselineIdeographic
  | AlignmentBaselineAlphabetic
  | AlignmentBaselineHanging
  | AlignmentBaselineMathematical
  deriving (Eq, Ord, Read, Show)

instance Monoid AlignmentBaseline where
  mappend x y
    | y == mempty = x
    | otherwise   = y
  mempty = AlignmentBaselineAuto

-- | Font style.
--
-- https://www.w3.org/TR/SVG/text.html#FontPropertiesUsedBySVG
data FontStyle
  = FontStyleNormal | FontStyleItalic | FontStyleOblique | FontStyleInherit
  deriving (Eq, Ord, Read, Show)

instance Monoid FontStyle where
  mappend x y
    | y == mempty = x
    | otherwise   = y
  mempty = FontStyleInherit

-- | Font weight.
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-weight
data FontWeight
  = FontWeightNormal
  | FontWeightBold
  | FontWeightBolder
  | FontWeightLighter
  | FontWeightN Int
  | FontWeightInherit
  deriving (Eq, Ord, Read, Show)

instance Monoid FontWeight where
  mappend x y
    | y == mempty = x
    | otherwise   = y
  mempty = FontWeightInherit

-- Font size
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/font-size
type FontSize = Str

-- | Text options. See 'textWithOptions'.
data TextOptions = TextOptions
  { textAnchor        :: !TextAnchor
  , alignmentBaseline :: !AlignmentBaseline
  , fontStyle         :: !FontStyle
  , fontFamily        :: !(First Str)
  , fontSize          :: !(First FontSize)
  , fontWeight        :: !FontWeight
  , textSelectable    :: !All
  }

-- | Left-biased. Mainly here for the 'mempty'.
instance Monoid TextOptions where
  mempty
    = TextOptions mempty mempty mempty mempty mempty mempty mempty
  mappend
    (TextOptions x1 x2 x3 x4 x5 x6 x7)
    (TextOptions y1 y2 y3 y4 y5 y6 y7)
      = TextOptions (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4) (x5 <> y5) (x6 <> y6) (x7 <> y7)
  -- TODO can we derive this?


textOptionsToStyle :: TextOptions -> Style
textOptionsToStyle opts = mconcat
  [ _fontWeight, _fontSize, _fontFamily, _fontStyle, _textAnchor
  , _alignmentBaseline, _textSelectable
  ]

  where
    _fontWeight = case fontWeight opts of
      FontWeightNormal          -> styleNamed "font-weight" "normal"
      FontWeightBold            -> styleNamed "font-weight" "bold"
      FontWeightBolder          -> styleNamed "font-weight" "bolder"
      FontWeightLighter         -> styleNamed "font-weight" "lighter"
      FontWeightInherit         -> styleNamed "font-weight" "inherit"
      FontWeightN n             -> styleNamed "font-weight" (toStr n)


    _fontSize = case fontSize opts of
      (First (Just v))          -> styleNamed "font-size" v
      _                         -> styleNamed "font-family" "10px"

    _fontFamily  = case fontFamily opts of
      (First (Just v))          -> styleNamed "font-family" v
      _                         -> styleNamed "font-family" "sans-serif"

    _fontStyle  = case fontStyle opts of
      FontStyleNormal           -> styleNamed "font-style" "normal"
      FontStyleItalic           -> styleNamed "font-style" "italic"
      FontStyleOblique          -> styleNamed "font-style" "oblique"
      FontStyleInherit          -> mempty

    _textAnchor = case textAnchor opts of
      TextAnchorStart           -> styleNamed "text-anchor"  "start"
      TextAnchorMiddle          -> styleNamed "text-anchor"  "middle"
      TextAnchorEnd             -> styleNamed "text-anchor"  "end"
      TextAnchorInherit         -> mempty

    _alignmentBaseline = case alignmentBaseline opts of
      AlignmentBaselineAuto           -> mempty
      AlignmentBaselineBaseline       -> styleNamed "dominant-baseline" "baseline"
      AlignmentBaselineBeforeEdge     -> styleNamed "dominant-baseline" "before-edge"
      AlignmentBaselineTextBeforeEdge -> styleNamed "dominant-baseline" "text-before-edge"
      AlignmentBaselineMiddle         -> styleNamed "dominant-baseline" "middle"
      AlignmentBaselineCentral        -> styleNamed "dominant-baseline" "central"
      AlignmentBaselineAfterEdge      -> styleNamed "dominant-baseline" "after-edge"
      AlignmentBaselineTextAfterEdge  -> styleNamed "dominant-baseline" "text-after-edge"
      AlignmentBaselineIdeographic    -> styleNamed "dominant-baseline" "ideographic"
      AlignmentBaselineAlphabetic     -> styleNamed "dominant-baseline" "alphabetic"
      AlignmentBaselineHanging        -> styleNamed "dominant-baseline" "hanging"
      AlignmentBaselineMathematical   -> styleNamed "dominant-baseline" "mathematical"

    {-
    *.unselectable {
       -moz-user-select: -moz-none;
       -khtml-user-select: none;
       -webkit-user-select: none;
       -ms-user-select: none;
       user-select: none;
    }
    -}
    _textSelectable = case textSelectable opts of
      All True  -> mempty
      All False -> mconcat
                    [ styleNamed "user-select"          "none"
                    , styleNamed "-moz-user-select"     "-moz-none"
                    , styleNamed "-khtml-user-select"   "none"
                    , styleNamed "-webkit-user-select"  "none"
                    , styleNamed "-ms-user-select"      "none"
                    -- Mouse pointer
                    , styleNamed "cursor"               "default"
                    ]
