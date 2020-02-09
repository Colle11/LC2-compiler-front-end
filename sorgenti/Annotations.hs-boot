
module Annotations where

data GenericAnnotated a e
data Ann

type Annotated e = GenericAnnotated Ann e
