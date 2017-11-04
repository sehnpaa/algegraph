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
said = Person "Said"
sara = Person "Sara"
stig = Person "Stig"
susanna = Person "Susanna"

-- Hasse
ester = Person "Ester Andersson"
lovisa = Person "Lovisa Andersson"
karl = Person "Karl Andersson"
kristina = Person "Kristina Johanna Maria"
andersAndersson = Person "Anders Andersson"
elsaAndersdotter = Person "Elsa Andersdotter"
andersOlsson = Person "Anders Olsson"
barbroPersdotter = Person "Barbro Persdotter"
olofLarsson = Person "Olof Larsson"
larsOlsson = Person "Lars Olsson"
mariaJacobsdotter = Person "Maria Jacobsdotter"
nilsJacobsson = Person "Nils Jacobsson"
annaCatrinaAndersdotter = Person "Anna Catrina Andersdotter"
jacobJacobsson = Person "Jacob Jacobsson"
jacobOlsson = Person "Jacob Olsson"
martaJacobsdotter = Person "Marta Jacobsdotter"
jacobJonsson = Person "Jacob Jönsson"
annaJonsdotter = Person "Anna Jönsdotter"
jonsJacobsson = Person "Jöns Jacobsson"
ingaJonsdotter = Person "Inga Jönsdotter"
jacobJonssonBanck = Person "Jacob Jonsson Banck"
kerstiOlsdotter = Person "Kersti Olsdotter"
jacobLarsson = Person "Jacob Larsson"
elinAndersdotter = Person "Elin Andersdotter"
olofHenriksson = Person "Olof Henriksson"
henrikOlsson = Person "Henrik Olsson"
kariJacobsdotter = Person "Kari Jacobsdotter"
larsLarsson = Person "Lars Larsson"
larsNilsson = Person "Lars Nilsson"
kerstinOlsdotter = Person "Kerstin Olsdotter"
nilsParsson = Person "Nils Pärsson"

-- Maj-britt
johanNiklasKarlsson = Person "Johan Niklas Karlsson"
carlAugustNiklasson = Person "Carl August Niklasson"
niklasNilsson = Person "Niklas Nilsson"
annaNilsdotter = Person "Anna Nilsdotter"
nilsAndersson = Person "Nils Andersson"


childrenOf :: Person -> [Person] -> Graph Person
childrenOf p = edges . zip (cycle [p])

allGs :: Graph Person
allGs = foldl overlay empty
  [ childrenOf anders [jessica]
  , childrenOf astor [kent, anders, stig]
  , childrenOf bodil [peter, lisa]
  , childrenOf hasse [bodil, lilliann, said]
  , childrenOf kent [peter, lisa]
  , childrenOf klara [kent, anders, stig]
  , childrenOf lilliann [karin, anna, sara]
  , childrenOf lisa [melissa]
  , childrenOf majbritt [bodil, lilliann]
  , childrenOf marcus [melissa]
  , childrenOf morgan [karin, anna, sara]
  , childrenOf susanna [melissa]
  -- Hasse
  , childrenOf ester [hasse]
  , childrenOf lovisa [ester]
  , childrenOf karl [ester]
  , childrenOf kristina [karl]
  , childrenOf andersAndersson [karl]
  , childrenOf elsaAndersdotter [andersAndersson]
  , childrenOf andersOlsson [andersAndersson]
  , childrenOf barbroPersdotter [andersOlsson]
  , childrenOf olofLarsson [andersOlsson]
  , childrenOf larsOlsson [olofLarsson]
    , childrenOf mariaJacobsdotter [lovisa]
    , childrenOf nilsJacobsson [lovisa]
    , childrenOf annaCatrinaAndersdotter [mariaJacobsdotter]
    , childrenOf jacobJacobsson [mariaJacobsdotter]
    , childrenOf martaJacobsdotter [jacobJacobsson]
    , childrenOf jacobOlsson [jacobJacobsson]
    , childrenOf jacobJonsson [martaJacobsdotter]
    , childrenOf annaJonsdotter [jacobJonsson]
    , childrenOf jonsJacobsson [jacobJonsson]
    , childrenOf ingaJonsdotter [jonsJacobsson]
    , childrenOf jacobJonssonBanck [jonsJacobsson]
    , childrenOf kerstiOlsdotter [nilsJacobsson]
    , childrenOf jacobLarsson [nilsJacobsson]
    , childrenOf elinAndersdotter [kerstiOlsdotter]
    , childrenOf olofHenriksson [kerstiOlsdotter]
    , childrenOf henrikOlsson [olofHenriksson]
    , childrenOf kariJacobsdotter [jacobLarsson]
    , childrenOf larsLarsson [jacobLarsson]
    , childrenOf larsNilsson [larsLarsson]
    , childrenOf kerstinOlsdotter [larsNilsson]
    , childrenOf nilsParsson [larsNilsson]
    -- Maj-britt
    , childrenOf johanNiklasKarlsson [majbritt]
    , childrenOf carlAugustNiklasson [johanNiklasKarlsson]
    , childrenOf niklasNilsson [carlAugustNiklasson]
    , childrenOf annaNilsdotter [niklasNilsson]
    , childrenOf nilsAndersson [annaNilsdotter]
  ]

k = childrenOf kent [peter, lisa]

-- g = foldg empty vertex (\a b -> if isSubgraphOf a k && isSubgraphOf b k then overlay a b else empty) connect allGs

ke = vertex kent

g = foldg empty vertex overlay (\a b -> if isSubgraphOf a ke || isSubgraphOf b ke then connect a b else empty) allGs


main = do
  writeFile "test" (export (defaultStyleViaShow {graphName = "G", vertexAttributes = (\p -> ["tooltip" := (show p ++ "!")])}) allGs )
