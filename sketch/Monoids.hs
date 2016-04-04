
instance Monoid T where
  mempty = T mempty
  mappend (T a16) (T b1) =
    T (a1 <> b1)

instance Monoid T where
  mempty = T mempty mempty
  mappend (T a1 a2) (T b1 b2) =
    T (a1 <> b1) (a2 <> b2)

instance Monoid T where
  mempty = T mempty mempty mempty
  mappend (T a1 a2 a3) (T b1 b2 b3) =
    T (a1 <> b1) (a2 <> b2) (a3 <> b3)

instance Monoid T where
  mempty = T mempty mempty mempty mempty
  mappend (T a1 a2 a3 a4) (T b1 b2 b3 b4) =
    T (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4)

instance Monoid T where
  mempty = T mempty mempty mempty mempty mempty
  mappend (T a1 a2 a3 a4 a5) (T b1 b2 b3 b4 b5) =
    T (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5)

instance Monoid T where
  mempty = T mempty mempty mempty mempty mempty mempty
  mappend (T a1 a2 a3 a4 a5 a6) (T b1 b2 b3 b4 b5 b6) =
    T (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)
