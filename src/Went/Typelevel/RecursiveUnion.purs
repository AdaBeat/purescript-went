module Went.Typelevel.RecursiveUnion where


import Prim.Row (class Union)
import Prim.RowList (Cons, Nil, RowList)

class RecursiveUnion (r1 :: RowList Type) (r2 :: RowList Type) (r3 :: RowList Type) | r1 r2 -> r3

instance RecursiveUnion Nil r2 r2
else instance RecursiveUnion r1 Nil r1
-- If a matching key is two records, resulting list is union of their fields
else instance
  ( Union r1 r2 r3
  , RecursiveUnion rest1AsList rest2AsList rest3AsList
  ) =>
  RecursiveUnion (Cons key (Record r1) rest1AsList) (Cons key (Record r2) rest2AsList) (Cons key (Record r3) rest3AsList)
-- Prioritize left non-record types in the case of same key
else instance
  ( RecursiveUnion rest1AsList rest2AsList rest3AsList
  ) =>
  RecursiveUnion (Cons key ty rest1AsList) (Cons key ty2 rest2AsList) (Cons key ty rest3AsList)
else instance 
  (RecursiveUnion rest1AsList rest2AsList rest3AsList
  ) =>
  RecursiveUnion (Cons key1 ty1 rest1AsList) (Cons key2 ty2 rest2AsList) (Cons key1 ty1 (Cons key2 ty2 rest3AsList))


type MyFields =
  ( key1 :: String
 -- , nested :: Record (nestedKey1 :: String, nestedKey2 :: String)
  )

type MyOtherFields =
  ( key2 :: Int
 -- , nested :: Record (nestedKey3 :: Int, nestedKey4 :: Int)
  )

-- call :: forall r. ListToRow (Cons "aaa" Int Nil) r => Record r
-- call = {aaa: 3}