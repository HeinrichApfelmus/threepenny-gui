{- Eliza: an implementation of the classic pseudo-psychoanalyst ---------------

    Gofer version by Mark P. Jones, January 12 1992.
    Modified for Hugs 1.3, August 1996
    Updated and cleaned to Haskell2010 by Heinrich Apfelmus, October 2017

    Previously adapted from a Pascal implementation provided as part of
    an experimental package from James Risner (risner@ms.uky.edu), Univ. of KY.
    with original Pascal code apparently provided by Robert Migliaccio
    (mig@ms.uky.edu).
----------------------------------------------------------------------------- -}

module Eliza where

import Data.Char (toUpper)

{-----------------------------------------------------------------------------
    Input / Output
------------------------------------------------------------------------------}
main = do
    putStr $ unlines
        [ "Hi! I'm Eliza. I am your personal therapy computer"
        , "Please tell me your problem."
        , ""
        ]
    session initialEliza

session :: Eliza -> IO ()
session s1 = do
    putStr "> "
    cs <- getLine
    let (response, s2) = eliza (words $ trim cs) s1
    putStrLn response
    session s2

-- strip punctuation characters
trim  :: String -> String
trim = reverse . dropPunct . reverse . dropPunct
    where
    dropPunct = dropWhile (`elem` punct)
    punct     = [' ', '.', '!', '?', ',']

{-----------------------------------------------------------------------------
    Eliza
    is essentially a state machine that maps words from the user input
    to responses.
------------------------------------------------------------------------------}
type Eliza = (State, Words)

eliza :: Words -> Eliza -> (String, Eliza)
eliza ws (s1,prev) = (response, (s2, ws))
    where (response, s2) = if prev == ws then repeated s1 else answer s1 ws

initialEliza :: Eliza
initialEliza = (initial, [])

answer                :: State -> Words -> (String, State)
answer st l            = (response, newKeyTab kt st)
 where (response, kt)         = ans (keyTabOf st)
       e `cons` (r, es)       = (r, e:es)
       ans (e:es) | null rs   = e `cons` ans es
                  | otherwise = (makeResponse a (head rs), (key,as):es)
                         where rs           = replies key l
                               (key,(a:as)) = e

-- Find all possible replies (without leading string for given key ------------

replies                 :: Words -> Words -> [String]
replies key l            = ( map (conjug l . drop (length key))
                           . filter (prefix key . map ucase)
                           . netails) l

prefix                  :: Eq a => [a] -> [a] -> Bool
[]     `prefix` xs       = True
(x:xs) `prefix` []       = False
(x:xs) `prefix` (y:ys)   = x==y && (xs `prefix` ys)

netails                 :: [a] -> [[a]]          -- non-empty tails of list
netails []               = []
netails xs               = xs : netails (tail xs)

ucase                   :: String -> String      -- map string to upper case
ucase                    = map toUpper

-- Replace keywords in a list of words with appropriate conjugations ----------

conjug     :: Words -> Words -> String
conjug d    = unwords . trailingI . map conj . maybe d  -- d is default input
              where maybe d xs = if null xs then d else xs
                    conj  w    = head ([m | (w',m)<-conjugates, uw==w'] ++ [w])
                                 where uw = ucase w
                    trailingI  = foldr cons []
                                 where x `cons` xs | x=="I" && null xs = ["me"]
                                                   | otherwise         = x:xs

conjugates :: [(String, String)]
conjugates = prepare (oneways ++ concat [[(x,y), (y,x)] | (x,y) <- bothways])
    where
    oneways  = [ ("me",   "you") ]
    bothways =
        [ ("are",  "am")    , ("we're", "was")
        , ("you",  "I" )    , ("your",  "my")
        , ("I've", "you've"), ("I'm",   "you're")
        ]
    prepare  = map (\(w,r) -> (ucase w, r))

-- Response data --------------------------------------------------------------

type Words    = [String]
type KeyTable = [(Key, Replies)]
type Replies  = [String]
type State    = (KeyTable, Replies)
type Key      = Words

repeated          :: State -> (String, State)
repeated (kt, (r:rp))      = (r, (kt, rp))

newKeyTab                 :: KeyTable -> State -> State
newKeyTab kt' (kt, rp)     = (kt', rp)

keyTabOf                  :: State -> KeyTable
keyTabOf (kt, rp)          = kt

makeResponse             :: String -> String -> String
makeResponse ('?':cs) us  = cs ++ " " ++ us ++ "?"
makeResponse ('.':cs) us  = cs ++ " " ++ us ++ "."
makeResponse cs       us  = cs

initial     :: State
initial      = ([(words k, cycle rs) | (k,rs) <-respMsgs], cycle repeatMsgs)

repeatMsgs   = [ "Why did you repeat yourself?",
         "Do you expect a different answer by repeating yourself?",
         "Come, come, elucidate your thoughts.",
         "Please don't repeat yourself!" ]

respMsgs     = [ ("CAN YOU",        canYou),
         ("CAN I",      canI),
         ("YOU ARE",        youAre),
         ("YOU'RE",     youAre),
         ("I DON'T",        iDont),
         ("I FEEL",     iFeel),
         ("WHY DON'T YOU",  whyDont),
         ("WHY CAN'T I",    whyCant),
         ("ARE YOU",        areYou),
         ("I CAN'T",        iCant),
         ("I AM",       iAm),
         ("I'M",        iAm),
         ("YOU",        you),
         ("YES",        yes),
         ("NO",         no),
         ("COMPUTER",       computer),
         ("COMPUTERS",      computer),
         ("I WANT",     iWant),
         ("WHAT",       question),
         ("HOW",        question),
         ("WHO",        question),
         ("WHERE",      question),
         ("WHEN",       question),
         ("WHY",        question),
         ("NAME",       name),
         ("BECAUSE",        because),
         ("CAUSE",      because),
         ("SORRY",      sorry),
         ("DREAM",      dream),
         ("DREAMS",     dream),
         ("HI",         hello),
         ("HELLO",      hello),
         ("MAYBE",      maybe),
         ("YOUR",       your),
         ("ALWAYS",     always),
         ("THINK",      think),
         ("ALIKE",      alike),
         ("FRIEND",     friend),
         ("FRIENDS",        friend),
         ("",           nokeyMsgs) ]
 where
  canYou     = [ "?Don't you believe that I can",
         "?Perhaps you would like to be able to",
         "?You want me to be able to" ]
  canI       = [ "?Perhaps you don't want to",
         "?Do you want to be able to" ]
  youAre     = [ "?What makes you think I am",
         "?Does it please you to believe I am",
         "?Perhaps you would like to be",
         "?Do you sometimes wish you were" ]
  iDont      = [ "?Don't you really",
         "?Why don't you",
         "?Do you wish to be able to",
         "Does that trouble you?" ]
  iFeel      = [ "Tell me more about such feelings.",
         "?Do you often feel",
         "?Do you enjoy feeling" ]
  whyDont    = [ "?Do you really believe I don't",
         ".Perhaps in good time I will",
         "?Do you want me to" ]
  whyCant    = [ "?Do you think you should be able to",
         "?Why can't you" ]
  areYou     = [ "?Why are you interested in whether or not I am",
         "?Would you prefer if I were not",
         "?Perhaps in your fantasies I am" ]
  iCant      = [ "?How do you know you can't",
         "Have you tried?",
         "?Perhaps you can now" ]
  iAm        = [ "?Did you come to me because you are",
         "?How long have you been",
         "?Do you believe it is normal to be",
         "?Do you enjoy being" ]
  you        = [ "We were discussing you --not me.",
         "?Oh,",
         "You're not really talking about me, are you?" ]
  yes        = [ "You seem quite positive.",
         "Are you Sure?",
         "I see.",
         "I understand." ]
  no         = [ "Are you saying no just to be negative?",
         "You are being a bit negative.",
         "Why not?",
         "Are you sure?",
         "Why no?" ]
  computer   = [ "Do computers worry you?",
         "Are you talking about me in particular?",
         "Are you frightened by machines?",
         "Why do you mention computers?",
         "What do you think machines have to do with your problems?",
         "Don't you think computers can help people?",
         "What is it about machines that worries you?" ]
  iWant      = [ "?Why do you want",
         "?What would it mean to you if you got",
         "?Suppose you got",
         "?What if you never got",
         ".I sometimes also want" ]
  question   = [ "Why do you ask?",
         "Does that question interest you?",
         "What answer would please you the most?",
         "What do you think?",
         "Are such questions on your mind often?",
         "What is it that you really want to know?",
         "Have you asked anyone else?",
         "Have you asked such questions before?",
         "What else comes to mind when you ask that?" ]
  name       = [ "Names don't interest me.",
         "I don't care about names --please go on." ]
  because    = [ "Is that the real reason?",
         "Don't any other reasons come to mind?",
         "Does that reason explain anything else?",
         "What other reasons might there be?" ]
  sorry      = [ "Please don't apologise!",
         "Apologies are not necessary.",
         "What feelings do you have when you apologise?",
         "Don't be so defensive!" ]
  dream      = [ "What does that dream suggest to you?",
         "Do you dream often?",
         "What persons appear in your dreams?",
         "Are you disturbed by your dreams?" ]
  hello      = [ "How do you...please state your problem." ]
  maybe      = [ "You don't seem quite certain.",
         "Why the uncertain tone?",
         "Can't you be more positive?",
         "You aren't sure?",
         "Don't you know?" ]
  your       = [ "?Why are you concerned about my",
         "?What about your own" ]
  always     = [ "Can you think of a specific example?",
         "When?",
         "What are you thinking of?",
         "Really, always?" ]
  think      = [ "Do you really think so?",
         "?But you are not sure you",
         "?Do you doubt you" ]
  alike      = [ "In what way?",
         "What resemblence do you see?",
         "What does the similarity suggest to you?",
         "What other connections do you see?",
         "Cound there really be some connection?",
         "How?" ]
  friend     = [ "Why do you bring up the topic of friends?",
         "Do your friends worry you?",
         "Do your friends pick on you?",
         "Are you sure you have any friends?",
         "Do you impose on your friends?",
         "Perhaps your love for friends worries you." ]

  nokeyMsgs    = [ "I'm not sure I understand you fully.",
         "What does that suggest to you?",
         "I see.",
         "Can you elaborate on that?",
         "Say, do you have any psychological problems?" ]
