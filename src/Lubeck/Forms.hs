
module Lubeck.Forms where

import Lubeck.FRP (Sink)
import Lubeck.Html (Html)

type Widget i o = Sink o -> i -> Html
type Widget' a  = Widget a a
