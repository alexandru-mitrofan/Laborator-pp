import Prelude hiding (getChar)
import Debug.Trace


data Trie = -- rădăcina are doar copii, nu conține niciun caracter
            Root { getChildren :: [Trie] }
            -- un nod conține un caracter și o listă de copii
            | Node {
                getChar :: Char,
                getChildren :: [Trie]
                }
            deriving (Show, Eq)

-- alternativ
data Trie1 = Node1 { getChar1 :: Maybe Char, getChildren1 :: [Trie1] }
-- sau getChar :: Char, și se pune '*' în rădăcină
-- sau alte soluții


emptyTrie = Root []


-- varianta A

canBeFollowedBy :: Char -> Trie -> Bool
-- iau caracterele din copii și mă uit dacă este vreun copil cu litera
canBeFollowedBy letter node = elem letter (map getChar $ getChildren node)

getSubtreeFor :: Char -> Trie -> Trie
getSubtreeFor letter node = if null selected then undefined else head selected
    where selected = filter ((==letter) . getChar) $ getChildren node

addWord :: [Char] -> Trie -> Trie
-- dacă s-a terminat cuvântul, atunci înseamnă că este deja în trie
addWord [] node = node
addWord (c:word) node
    -- dacă există deja următorul simbol într-un nod copil
    | canBeFollowedBy c node = constr $ 
                filter ((/= c) . getChar) chd
                ++
                -- adaug în nodul corespunzător lui c sufixul care a mai rămas
                [addWord word $ getSubtreeFor c node]
    -- altfel, adaug un nod nou, ca copil al nodului curent
    | otherwise = constr $ addWord word (Node c []) : chd
    where
        -- abstractizez cazurile
        constr = case node of Node cn _ -> Node cn; otherwise -> Root
        chd = getChildren node


-- varianta B

countWords :: Num p => Trie -> p
countWords node = 
    if null $ getChildren node then 1 -- o frunză -> un cuvânt
    else sum $ map countWords $ getChildren node -- sumă peste cuvintele din toți copiii

printWords :: Trie -> [[Char]]
-- întorc mereu o listă de cuvinte (stringuri)
printWords (Root chd) = concatMap printWords chd
printWords (Node c chd) = if null chd then [[c]]
    -- iau toate cuvintele din subarbori și adaug c în fața fiecăruia
    else map (c:) $ concatMap printWords chd

-- dacă s-a terminat prefixul, iau toate sufixele care se pot forma din acest punct
predict [] node = printWords node
predict pref@(c:rest) node = case node of
    -- dacă este rădăcină sau nodul are ca valoare primul caracter, continui
    Root _ -> concatMap (predict pref) $ getChildren node
    _ -> if c == getChar node then
        -- concatenez soluțiile pentru toți copiii și pun c în față
            map (c:) $ concatMap (predict rest) $ getChildren node
        -- altfel, întorc o listă vidă de cuvinte din acest nod
        else []
        

-- funcție ajutătoare
addWords wordlist = foldl (flip addWord) emptyTrie wordlist






