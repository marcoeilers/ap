import qualified Data.Map as Map
import Data.Char (toUpper)
import Data.Tuple (swap)
import Data.List (nub, partition,isPrefixOf)

morseTable = Map.fromList [ ('A', ".-")
                          , ('B', "-...")
                          , ('C', "-.-.")
                          , ('D', "-..")
                          , ('E', ".")
                          , ('F', "..-.")
                          , ('G', "--.")
                          , ('H', "....")
                          , ('I', "..")
                          , ('J', ".---")
                          , ('K', "-.-")
                          , ('L', ".-..")
                          , ('M', "--")
                          , ('N', "-.")
                          , ('O', "---")
                          , ('P', ".--.")
                          , ('Q', "--.-")
                          , ('R', ".-.")
                          , ('S', "...")
                          , ('T', "-")
                          , ('U', "..-")
                          , ('V', "...-")
                          , ('W', ".--")
                          , ('X', "-..-")
                          , ('Y', "-.--")
                          , ('Z', "--..")
                          ]

revMorseTable = Map.fromList $ map swap $ Map.assocs morseTable

encode :: String -> String
encode msg = concat morseLetters
  where morseLetters = [ s | (Just s) <- translate msg ]
        translate = map (\c -> Map.lookup (toUpper c) morseTable)

-- decode :: String -> [String]
decode' [] = []
decode' msg = [ (letter, rest)  | (letter, code) <- Map.assocs morseTable
                                , code `isPrefixOf` msg
                                , rest <- drop (length code) msg
                                ]

-- exploring alternatives, look at the first, see if match 
decode :: String -> [String]
decode []  = []
decode msg = map (reverse . fst) $ concat $ realDecode [([], msg)]
  
  
-- Takes a list of partially decoded messages
realDecode :: [(String, String)] -> [[(String, String)]]
realDecode [] = []
realDecode toDecode = finished : realDecode unfinished
  where processed = concatMap (uncurry decodingStep) toDecode
        (finished, unfinished) = partition (\(x,y) -> null y) processed

-- The decoded string is returned in reverse
decodingStep :: String -> String -> [(String, String)]
decodingStep decoded restOfMsg = nextDecoded
  where nextDecoded = nub [ (c : decoded, rest) | (Just c, rest) <- inits  ]
        inits = map (decodePair . chop) [1..4]
        decodePair (c,t) = (lookup c, t)
        chop = (flip splitAt) restOfMsg
        lookup = (flip Map.lookup) revMorseTable
