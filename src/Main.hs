module Main where

import Algebra.Graph
import Algebra.Graph.Export.Dot

data Person = Person
  { name :: String }
  deriving (Eq, Ord)

instance Show Person where
  show = name

anders = Person "Anders"
anna = Person "Anna"
astor = Person "Astor"
bodil = Person "Bodil"
hasse = Person "Hasse"
jessica = Person "Jessica"
karin = Person "Karin"
kent = Person "Kent"
klara = Person "Klara"
lilliann = Person "Lilliann"
lisa = Person "Lisa"
majbritt = Person "Maj-Britt"
marcus = Person "Marcus"
melissa = Person "Melissa"
morgan = Person "Morgan"
peter = Person "Peter"
sara = Person "Sara"
stig = Person "Stig"
susanna = Person "Susanna"

childrenOf :: Person -> [Person] -> Graph Person
childrenOf p = edges . zip (cycle [p])

allGs :: Graph Person
allGs = foldl overlay empty
  [ childrenOf anders [jessica]
  , childrenOf astor [kent, anders, stig]
  , childrenOf bodil [peter, lisa]
  , childrenOf hasse [bodil, lilliann]
  , childrenOf kent [peter, lisa]
  , childrenOf klara [kent, anders, stig]
  , childrenOf lilliann [karin, anna, sara]
  , childrenOf lisa [melissa]
  , childrenOf majbritt [bodil, lilliann]
  , childrenOf marcus [melissa]
  , childrenOf morgan [karin, anna, sara]
  , childrenOf susanna [melissa] ]

k = childrenOf kent [peter, lisa]

-- g = foldg empty vertex (\a b -> if isSubgraphOf a k && isSubgraphOf b k then overlay a b else empty) connect allGs

ke = vertex kent

g = foldg empty vertex overlay (\a b -> if isSubgraphOf a ke || isSubgraphOf b ke then connect a b else empty) allGs



main = do
  writeFile "test" (export (defaultStyleViaShow {graphName = "G", vertexAttributes = (\p -> ["tooltip" := (show p ++ "!")])}) allGs )
