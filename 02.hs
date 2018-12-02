import Data.Map (Map)
import qualified Data.Map as Map

-- Section 1

hasDoubleLetter :: String -> Bool
hasDoubleLetter str = hasValue (Map.toList $ toMap str) 2

hasTripleLetter :: String -> Bool
hasTripleLetter str = hasValue (Map.toList $ toMap str) 3

toMap :: (Ord a) => [a] -> Map a Int
toMap xs = toMapFull xs (Map.empty)

toMapFull :: (Ord a) => [a] -> Map a Int -> Map a Int

toMapFull [] map = map
toMapFull (x:xs) map
    | Map.lookup x map == Nothing = toMapFull xs (Map.insert x 1 map)
    | otherwise = toMapFull xs (Map.insertWith (+) x 1 map)

hasValue :: (Ord a, Eq b) => [(a,b)] -> b -> Bool

hasValue [] _ = False
hasValue ( (_,b):xs ) val
    | val == b = True
    | otherwise = hasValue xs val 

performChecksum :: [String] -> Int
performChecksum xs = doubles * triples
    where doubles = sum . (map (\x -> if x then 1 else 0)) $ map hasDoubleLetter xs
          triples = sum . (map (\x -> if x then 1 else 0)) $ map hasTripleLetter xs

{- Solution (4920)
performChecksum ids
-}

-- Section 2

offByOneLetter :: String -> String -> Bool

offByOneLetter xs ys = sum (map (\(x,y) -> if x == y then 0 else 1) (zip xs ys)) == 1

letterDiff :: String -> String -> String

letterDiff _ [] = []
letterDiff [] _ = []
letterDiff (x:xs) (y:ys)
    | x == y = x : letterDiff xs ys
    | otherwise = letterDiff xs ys

compareAgainstAll :: [a] -> (a -> a -> Bool) -> [(a,a)]

compareAgainstAll (_:[]) _ = []
compareAgainstAll [] _ = []
compareAgainstAll (x:xs) f = (map (\y -> (x,y)) (filter (\y -> f x y) xs)) ++ (compareAgainstAll xs f)

{- Solution (fonbwmjquwtapeyzikghtvdxl)
map (\(a,b) -> letterDiff a b) (compareAgainstAll ids offByOneLetter)
-}

-- Given Info
ids = 
    ["fonbsmjyqugrapsczckghtvdxl",
    "fonpsmjyquwrnpeczikghtvdxw",
    "fonbsmdymuwrapexzikghtvdxl",
    "fonwsmjyquwrapeczikghttdpl",
    "fonbsmjkquwrapeczjkghtvdxx",
    "yonbsmjyquwrapecgikghtvdxc",
    "donbsmjyquqrapeczikghtadxl",
    "monbsmjyquprgpeczikghtvdxl",
    "fonbsmjyquwvapecqgkghtvdxl",
    "fonbsmjyquwrkphczikghsvdxl",
    "fonbomjyeuwvapeczikghtvdxl",
    "fonwsmjyjuwrapoczikghtvdxl",
    "foybsmjyquwcapeczikghsvdxl",
    "fonbsmjyquwrtaeczikgptvdxl",
    "ponbsmpyquwjapeczikghtvdxl",
    "flnbcmjyquwrqpeczikghtvdxl",
    "fonbsmjyquwrapegzikvbtvdxl",
    "fonbjmjyqgwrazeczikghtvdxl",
    "zoabsmjyquwkapeczikghtvdxl",
    "fonbsmjyquwrapecziktxkvdxl",
    "fonbsxjyrpwrapeczikghtvdxl",
    "fonbsmjbquwqapeciikghtvdxl",
    "lonbsmjyquwraphczikghtvdul",
    "ftnbsmjyquwrapcczikghtxdxl",
    "fonbsmjyqgwrapeczikghtldxc",
    "fonbsmjsquwmapeyzikghtvdxl",
    "fonbsmjyqfwrapecziqghtgdxl",
    "yonbsmjyquwraveczikgftvdxl",
    "fovbsmjyquwrapeczikggkvdxl",
    "fonbsmjyquwrapezzikghbvdvl",
    "fonzsmxyquwrapeczukghtvdxl",
    "fonbemjyquwrapevzikghtvrxl",
    "conbsxjxquwrapeczikghtvdxl",
    "fonbsmjsmewrapeczikghtvdxl",
    "folbsmjyqhwrapqczikghtvdxl",
    "fonbsmjyquwrzneczikghtvdxn",
    "fonbsmjyquirapeczikjhtvdll",
    "fontsmgyquwrgpeczikghtvdxl",
    "fonbsmjyauwrapeczbfghtvdxl",
    "ftnbsmjyquwrapecpifghtvdxl",
    "fonvsmjyqewrapeczikghlvdxl",
    "fonbsljyquwrapecziklhtvdxw",
    "fonbbmjyquwrapeczikghvadxl",
    "ponbsmjyquwrspeczikghivdxl",
    "fonbsmjcquwrapeccikghtvuxl",
    "fonbsmjnquwrapetzikghtvlxl",
    "fonbsmjymuwrapeczieghtvdxr",
    "ffnbsxnyquwrapeczikghtvdxl",
    "fonbsmjytuwrajeczzkghtvdxl",
    "fonssmjyquwhapeczikghkvdxl",
    "fonbsajyuuwrapeczikghlvdxl",
    "fonbsmjyquwrapeczihghtcixl",
    "fohbsmjyquwrapzczirghtvdxl",
    "fonbsmjyquwrapecjqnghtvdxl",
    "fonbsmjytuhrapeczihghtvdxl",
    "foabumjyquwrapeczikghtvdxz",
    "conbsmjyqtwrapeczikggtvdxl",
    "fonbsmjyiqwrapeczokghtvdxl",
    "fondsmjypuwrapeczikghtvjxl",
    "fonbswjyquwrapeczikgvtydxl",
    "fonbsmjyqqwrapeczikkhtvdbl",
    "fonbsmjyquwrapemzitghtvdsl",
    "fonbsmjyquwrspecziegxtvdxl",
    "fonbsmpyquwrgpeczikghtwdxl",
    "fodbsmjqquwrapeczmkghtvdxl",
    "fonbsmjkquwrapeczikghpvdxr",
    "fonbsmjyquwrapeczikshzvmxl",
    "fznbsmjyqulrapeczikghkvdxl",
    "fonbsmjyquwripeczikghtbdjl",
    "fcnbsmjyquzrapecyikghtvdxl",
    "ronbxmjyquwrapeczikghgvdxl",
    "fonbsmuyvuwrgpeczikghtvdxl",
    "fonbsmjyyuwraplczikghtudxl",
    "poxbsmjyqewrapeczikghtvdxl",
    "foabsmjyquwrapecziqghtvpxl",
    "ponbsmjrquwrapeczikchtvdxl",
    "fonzzmjyquwrapeczikghtvdxs",
    "wonbsmjyquwghpeczikghtvdxl",
    "fofbsejyquwrapeczikgctvdxl",
    "ponbsmjyquwrayegzikghtvdxl",
    "fonbumjyquwripeczikghtvdxf",
    "fonbsmqyquwrapeczikgftvdxv",
    "qonbsmjyquwraplczitghtvdxl",
    "fmnbsajdquwrapeczikghtvdxl",
    "fonbsrjyquwrapempikghtvdxl",
    "fonbsmjyquwrapeczikgotudxw",
    "fonbsmtyquwrapeflikghtvdxl",
    "fzqbsmjyquwrapecjikghtvdxl",
    "fdnbsmjyquwraqeclikghtvdxl",
    "fvnbsijyquwrapechikghtvdxl",
    "fovbsmjyquwsapeczikghqvdxl",
    "ffjbsmjyqgwrapeczikghtvdxl",
    "fonbsmjyquwrapeczvkhhivdxl",
    "forbamjjquwrapeczikghtvdxl",
    "fonbwmjyquwtapeyzikghtvdxl",
    "fonvsmjyquwrapeczikglnvdxl",
    "fonnsmjyguwrapeczikghtvxxl",
    "fopbsmjyquwrapeczikghtvaxz",
    "fonbsmjyquwiapeczikrhavdxl",
    "fonbsujyquwrapeczikthtvdjl",
    "fonpsmkyeuwrapeczikghtvdxl",
    "fonbsmjyquwrapeczqkgttvdxk",
    "fonbsmjyqzwrapeczikgrtddxl",
    "fokbsmjiquwrapeczikgltvdxl",
    "fonbsmjyqbwrapeczikghttdxo",
    "fonbsejyquwrapeczikghbvdal",
    "fonblmjyquwyaveczikghtvdxl",
    "fonbsmjyquwlzpepzikghtvdxl",
    "fonbsmjyqulrapbczigghtvdxl",
    "fonbsmjyxuwrapecziyghtvsxl",
    "fonbyjjyquwrapeczikghtvdxn",
    "fonbhmjyquwrapeczikghtjhxl",
    "fonbspjykuwraieczikghtvdxl",
    "aonbsmjyquwwapeczikchtvdxl",
    "fombsmjyquwyapeczikghtvdll",
    "fonbsmjynuwrapeczivgbtvdxl",
    "xonbsmjfquwrapeczikghqvdxl",
    "fonbyzjyquwzapeczikghtvdxl",
    "fbnbsmjyquwrapeczimgvtvdxl",
    "qonbsmjyquwraoeczikgftvdxl",
    "fonbsrjyquwrapeczikghtvjxm",
    "fonbsmjyquwrapxjzykghtvdxl",
    "fonbwgjyquwrapecziklhtvdxl",
    "fonjcmjyouwrapeczikghtvdxl",
    "fonbsmjyquwrapefzisuhtvdxl",
    "fonbsmjyqywrspeczikghtvnxl",
    "qonbsmjyquwrapeczlkuhtvdxl",
    "fonbsmjyqlprapeczikghtvdbl",
    "fonbsmjzquwrapedzikfhtvdxl",
    "fonbsmjyquwrapeczizghtvjxq",
    "fonbsmxyquwrrpeczikghtvcxl",
    "fonpsmjyquwoapeczikghjvdxl",
    "fonbshkyauwrapeczikghtvdxl",
    "fonbsmjysuwrapeczilghpvdxl",
    "fovwsxjyquwrapeczikghtvdxl",
    "fonbsmjyquwrppecnikghmvdxl",
    "fonbkmjyiuwrrpeczikghtvdxl",
    "gonbsmjyquwrapeczikphtudxl",
    "foncsmjyqlwrapeczimghtvdxl",
    "fonbsmjhquwrtpeczikghtvdxg",
    "fogbsmjyquarapeczikghtvdil",
    "fonbsmjyquwraperzekghwvdxl",
    "fonbstjyquwrapeczicghtedxl",
    "fonbsmjoquhrapeczikgotvdxl",
    "fonbsmjykuwrareczikgdtvdxl",
    "fonbsmjyvuwrayeczivghtvdxl",
    "fonbzmgyquwraptczikghtvdxl",
    "fonbsmjyqubrapeczikgftvdxb",
    "fonbgmjyjuwrapeczikghtvdul",
    "fonbsmjzqurrapeczikghtvfxl",
    "fonbsmjyiuwrapeczikgstvtxl",
    "fpnbstjyquwrapeczikghtvdcl",
    "fonbpmjyquwrapeczivghtndxl",
    "fonbsmjyquwrapeczilgptvvxl",
    "fonbsmjyqdwripecbikghtvdxl",
    "fonbsmjytuwgapnczikghtvdxl",
    "fonbsejyquwrapedzikghtvdml",
    "fonbsojyqdwrapeczikghtgdxl",
    "fonbsmjykuwrayeczicghtvdxl",
    "foubsmtyquwrapeczikchtvdxl",
    "fonbqmjyqukrapeyzikghtvdxl",
    "fonbsmjyquwaapenzikghtvdwl",
    "fonbsmeyquwrapeyzixghtvdxl",
    "fonusmjyquhrapeczikgytvdxl",
    "fonbsmjyquwrapwazikqhtvdxl",
    "fonwsmeyquwrapeczikghhvdxl",
    "fonmsmjyquxrspeczikghtvdxl",
    "fonqsmjyqxwrapeczikghtvdml",
    "fonfsmjyquwrapeuzikgatvdxl",
    "fonvsmjyquwrapeczikgrtvdul",
    "fonbsmayquwrapeczikihtvdxm",
    "fonbsmnyquwrapecdifghtvdxl",
    "fonbsmjyeuwraseczikghtvdxo",
    "fonbvvjyquwrapeczikghtvdxi",
    "fonbsmjyquwrapeczbkghtorxl",
    "tonbsmjyqvwrapeczikghtvdcl",
    "fonbsmjyquwrapeczhkgbtvdkl",
    "fonqsmjyquwrapenzibghtvdxl",
    "fontsmeyqudrapeczikghtvdxl",
    "qonbsmjyauwrapeczikghtvdbl",
    "fynbsmjyluwrapeczekghtvdxl",
    "fonbsmjhquwrappczikghtvdxt",
    "conbsmjyquwrapeczikahtvdxz",
    "fonbsmjyquorapeczikvftvdxl",
    "fonbsriyquwrapeczikchtvdxl",
    "yonfsmjyquwrapeczikghtvdxq",
    "fonaomjyquwrapecziwghtvdxl",
    "fonbsxsyqdwrapeczikghtvdxl",
    "fonbsqjyouwrapeczikgltvdxl",
    "fonbstsyquwraleczikghtvdxl",
    "fonbsmjyquwraoecztkghtvdsl",
    "fonbsmjyquwrapezzjkghmvdxl",
    "fonbwmjyqnwrapecpikghtvdxl",
    "fonbsmvyqbwrapeczikghtvdsl",
    "fonbsijyquwrazeczikghtvdwl",
    "fonbsmjyouwrapewzikghtldxl",
    "xonbsmjyqcwrapeczikghtvdul",
    "fonbgmjxquwrajeczikghtvdxl",
    "fokbsmjyquwrapechikghtrdxl",
    "fonbqmjyqawrapeczikghtrdxl",
    "fonbwmjzquwtapeyzikghtvdxl",
    "fonbsmjyquwrapecdikgatvdnl",
    "fonbsmjyqowrkpeczikghtvdxj",
    "fonbsmjyquwkapejzikuhtvdxl",
    "fonbsmjyquwrabeozikghtmdxl",
    "fonbsijyeuwrapeczikghtvdxh",
    "fonbsmjhquprapeczizghtvdxl",
    "fonesmjyquwrapcczikghtvdxh",
    "fonbamjyquwrapeczifrhtvdxl",
    "foabsmjyquwpapeczikghtvdxs",
    "fonbsmjyquwrapeczukghivdxh",
    "fonbsejyoulrapeczikghtvdxl",
    "fonbsmjyquwraceczikgdmvdxl",
    "eonbsmjyquerppeczikghtvdxl",
    "ffnzsmjyquwgapeczikghtvdxl",
    "donbsmyyquwrapeczirghtvdxl",
    "fjnbsmjyqufrapeczikghtwdxl",
    "fonfsmjyquwrareczigghtvdxl",
    "fonusmjyquwrapeczikgetvexl",
    "tonbsmjyqpwrapeczikghtjdxl",
    "fonbsmjhqukkapeczikghtvdxl",
    "fonbsmjyqusraseczikghtvzxl",
    "fonbsmjyquygapeczxkghtvdxl",
    "folbsmjyquwraqeczikghjvdxl",
    "fonbsmjyquwrppecjinghtvdxl",
    "fonbsmjyquwraepczhkghtvdxl",
    "fonbfmjyquwrapeczisghtrdxl",
    "fsnbsmjwqubrapeczikghtvdxl",
    "fonbspjyquwrapjczikghtedxl",
    "fowbsmjyquwrapeczikghtbdbl",
    "fonbymjyquwrapeczikghlvdrl",
    "fonbsmjyruwrapecbikghtvixl",
    "fonyqmjyqufrapeczikghtvdxl",
    "focbscjyquwrapeczmkghtvdxl",
    "fonbsmjyqtwnkpeczikghtvdxl",
    "eonbsmjyquwrameczizghtvdxl",
    "zonbsmjyqcwrapeczikghtvhxl",
    "foubsmjyquwrapehzikghtvnxl",
    "ffnbsmjyquwrapetzikghtjdxl",
    "fonbjgjyquwrapkczikghtvdxl",
    "fonbwmjyquwqapeczdkghtvdxl",
    "forbsmjyquwrapeczikkhtvdml",
    "fonbsmjyiuwrapeczivghevdxl",
    "fonbsmjyquwrapeglikghwvdxl",
    "fopgsmjyquwrapegzikghtvdxl",
    "fonbsmjyqzwrajeczikghtldxl",
    "fonbsmjyruwrapexzmkghtvdxl",
    "fonbsmjyquwrdpeczikxstvdxl",
    "fonbsmjyquwrapeezivghtvdql",
    "fonbdmjyqujsapeczikghtvdxl"]