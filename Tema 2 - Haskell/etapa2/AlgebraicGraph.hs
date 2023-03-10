module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}
nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = S.empty
nodes (Node a) = S.fromList[a]
nodes (Overlay a1 a2) = S.union(nodes a1) (nodes a2)
nodes (Connect a1 a2) = S.union(nodes a1) (nodes a2)
{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}
edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = S.empty
edges (Node a) = S.empty
edges (Overlay a1 a2) = S.union(edges a1) (edges a2)
edges (Connect a1 a2) = S.union (S.cartesianProduct(nodes a1) (nodes a2)) (S.union(edges a1) (edges a2))

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
    outNeighbors node graph = undefined
     S.union (S.cartesianProduct(nodes a1) (nodes a2)) (S.union(edges a1) (edges a2))
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors _ Empty = S.empty
outNeighbors _ (Node a) = S.empty
outNeighbors node (Overlay a1 a2) = S.union(outNeighbors node a1) (outNeighbors node a2)
outNeighbors node (Connect a1 a2) = if S.member node (nodes a1) then S.union (nodes a2) (S.union(outNeighbors node a1) (outNeighbors node a2)) 
else S.union(outNeighbors node a1) (outNeighbors node a2)
    
    
   

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors _ Empty = S.empty
inNeighbors _ (Node a) = S.empty
inNeighbors node (Overlay a1 a2) = S.union(inNeighbors node a1) (inNeighbors node a2)
inNeighbors node (Connect a1 a2) = if S.member node (nodes a2) then S.union (nodes a1) (S.union(inNeighbors node a1) (inNeighbors node a2)) 
else S.union(inNeighbors node a1) (inNeighbors node a2)

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
    removeNode node graph = undefined
-}
removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode _ Empty = Empty
removeNode node (Node a) = if a == node then Empty else Node a
removeNode node (Overlay a1 a2) = Overlay(removeNode  node a1) (removeNode node a2)
removeNode node (Connect a1 a2) = Connect(removeNode  node a1) (removeNode node a2)
{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
    splitNode old news graph = undefined
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news (Node a) 
    | a == old = foldr (\x acc -> Overlay (Node x) acc) Empty news
    | otherwise = (Node a)
splitNode old news (Overlay a1 a2) = Overlay (splitNode old news a1) (splitNode old news a2)
splitNode old news (Connect a1 a2) = Connect (splitNode old news a1) (splitNode old news a2)

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
    mergeNodes prop node graph = undefined
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut

mergeNodes prop node (Node a) 
    | prop a = Node node
    | otherwise = Node a
mergeNodes prop node (Overlay a1 a2) = Overlay (mergeNodes prop node a1) (mergeNodes prop node a2)
mergeNodes prop node (Connect a1 a2) = Connect (mergeNodes prop node a1) (mergeNodes prop node a2)