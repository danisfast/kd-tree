open Base
module Dimension = Dimension
module Scalar = Scalar
module Builder = Kd_tree_builder
module Int_2d = Kd_tree_builder.Configure (Int) (Dimension.Two)
module Int_3d = Kd_tree_builder.Configure (Int) (Dimension.Three)
module Float_2d = Kd_tree_builder.Configure (Float) (Dimension.Two)
module Float_3d = Kd_tree_builder.Configure (Float) (Dimension.Three)
