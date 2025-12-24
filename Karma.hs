module Karma where

import Control.Monad (unless, when)
import Control.Monad.State
import Data.List (delete, elemIndex, find, groupBy, maximumBy, minimumBy, sortBy, sortOn, (\\))
import Data.Maybe (listToMaybe)
import Data.Ord
import System.Random

--------------------------------------------------------------------------------
-- DATA TYPES
--------------------------------------------------------------------------------

-- | Represents the four suits in a standard deck
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Represents card ranks from 2 to Ace
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | A playing card with rank and suit
data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Eq, Show, Read)

-- | Collection of cards
type Deck = [Card]

-- | Discard pile
type Pile = [Card]

-- | Unique player identifier
type PlayerId = Int

-- | Player display name
type PlayerName = String

-- | Represents a player with their cards in different zones
data Player = Player
  { pId :: PlayerId,
    pName :: PlayerName,
    hand :: [Card],
    faceUp :: [Card],
    faceDown :: [Card]
  }

instance Show Player where
  show p =
    "Player{"
      ++ "id="
      ++ show (pId p)
      ++ ", name='"
      ++ pName p
      ++ "'"
      ++ ", hand="
      ++ show (length (hand p))
      ++ " cards"
      ++ ", faceUp="
      ++ show (length (faceUp p))
      ++ " cards"
      ++ ", faceDown="
      ++ show (length (faceDown p))
      ++ " cards"
      ++ "}"

-- | Complete game state
data GameState = GameState
  { players :: [Player],
    currentIx :: Int,
    drawPile :: Deck,
    discardPile :: Pile,
    burnedPiles :: [Pile],
    rng :: StdGen,
    finishedOrder :: [PlayerId],
    activeExtensions :: [Extension]
  }
  deriving (Show)

-- | Optional rule extensions
data Extension = ExtReverse8 | ExtThree3s | ExtNineClubs
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- STEP 1: CORE RULES & ENGINE HELPERS
--------------------------------------------------------------------------------

-- | Determines if a card can legally be played on top of another card
legalPlay :: Maybe Card -> Card -> Bool
legalPlay Nothing _ = True -- Empty pile: any card is legal
legalPlay (Just topCard) playCard
  | rank playCard == R2 = True -- 2 always legal (reset)
  | rank playCard == R8 = True -- 8 always legal (transparent)
  | rank playCard == R10 = True -- 10 always legal (burn)
  | rank topCard == R7 = rank playCard <= R7 -- After 7 only ≤7 allowed
  | otherwise = rank playCard >= rank topCard -- Normal play rank must be ≥

-- | Filters a deck to only cards that are legal to play on the given top card
validPlays :: Maybe Card -> Deck -> Deck
validPlays topCard = filter (legalPlay topCard)

-- | Removes n cards from the draw pile and returns them
dealCards :: Int -> State GameState Deck
dealCards n = state $ \gs ->
  let (taken, remaining) = splitAt n (drawPile gs)
   in (taken, gs {drawPile = remaining})

-- | Gives the entire discard pile to a player and clears it
giveWastePileTo :: Player -> State GameState ()
giveWastePileTo recipient = modify $ \gs ->
  let pile = discardPile gs
      recipientId = pId recipient
      updatePlayer p =
        if pId p == recipientId
          then p {hand = hand p ++ pile} -- Add discard pile to hand
          else p
   in gs
        { players = map updatePlayer (players gs),
          discardPile = [] -- Clear discard pile
        }

-- | Draws cards from draw pile until player has exactly 3 hand cards
replenishCards :: Player -> State GameState ()
replenishCards player = do
  gs <- get
  let handSize = length (hand player)
      needed = max 0 (3 - handSize) -- Calculate how many cards needed
      targetId = pId player

  when (needed > 0 && not (null (drawPile gs))) $ do
    let available = length (drawPile gs)
        toDraw = min needed available -- Draw up to available cards
    newCards <- dealCards toDraw

    modify $ \gs ->
      let update p =
            if pId p == targetId
              then p {hand = hand p ++ newCards}
              else p
       in gs {players = map update (players gs)}

-- | Shuffles a deck using a random number generator
shuffleDeck :: StdGen -> Deck -> Deck
shuffleDeck gen deck =
  let n = length deck
      keys = take n (randoms gen :: [Int]) -- Generate random keys
      shuffled = map snd $ sortOn fst (zip keys deck) -- Sort by random keys
   in shuffled

--------------------------------------------------------------------------------
-- STEP 2: BASIC PLAYER AND GAME LOOP
--------------------------------------------------------------------------------

-- | Retrieves the current player from game state
getCurrentPlayer :: State GameState (Maybe Player)
getCurrentPlayer = do
  gs <- get
  let ps = players gs
      ix = currentIx gs
  return $
    if null ps || ix < 0 || ix >= length ps
      then Nothing
      else Just (ps !! ix) -- Return player at current index

-- | Basic strategy: plays lowest legal single card from hand/face-up, or random face-down
basicStrategy :: State GameState Deck
basicStrategy = do
  mplayer <- getCurrentPlayer
  gs <- get
  playMove mplayer gs
  where
    playMove Nothing _ = return []
    playMove (Just player) gs
      | not (null handLegal) = return [minimumBy (comparing rank) handLegal]
      | canPlayFaceUp player gs = return [minimumBy (comparing rank) faceUpLegal]
      | canPlayFaceDown player = pickRandomFaceDown player
      | otherwise = return []
      where
        top = listToMaybe (discardPile gs)
        handLegal = validPlays top (hand player)
        faceUpLegal = validPlays top (faceUp player)

        canPlayFaceUp p gs =
          null (hand p) && null (drawPile gs) && not (null faceUpLegal)
        canPlayFaceDown p =
          null (hand p) && null (faceUp p) && not (null (faceDown p))
        pickRandomFaceDown p = do
          let cards = faceDown p
              (idx, newGen) = randomR (0, length cards - 1) (rng gs)
          put $ gs {rng = newGen}
          return [cards !! idx]

-- | Executes one complete turn for the current player using basic strategy
applyStrategy :: State GameState ()
applyStrategy = do
  gs <- get
  let player = players gs !! currentIx gs

  play <- basicStrategy -- Get strategy's recommended play
  if null play
    then giveWastePileTo player -- No legal play pick up discard pile
    else do
      modify $ \gs ->
        let idx = currentIx gs
            remove zone = zone \\ play
            updatedPlayer -- Determine which zone cards came from
              | play `subset` hand player = player {hand = remove (hand player)}
              | play `subset` faceUp player = player {faceUp = remove (faceUp player)}
              | otherwise = player {faceDown = remove (faceDown player)}
            players' = take idx (players gs) ++ [updatedPlayer] ++ drop (idx + 1) (players gs)
         in gs
              { players = players',
                discardPile = discardPile gs ++ play
              }

      processAfterPlayEffects play -- Handle burns and special effects
  replenishCards player
  advanceToNextPlayer
  where
    subset xs ys = all (`elem` ys) xs

    -- \| Handles burn effects and special card rules
    processAfterPlayEffects :: Deck -> State GameState ()
    processAfterPlayEffects played = do
      gs <- get
      let exts = activeExtensions gs

      -- 8 reverses direction (if extension active)
      when (ExtReverse8 `elem` exts && any (\c -> rank c == R8) played) $ modify $ \gs ->
        let n = length (players gs)
            oldIdx = currentIx gs
            newIdx = n - 1 - oldIdx -- Mirror index for reversal
         in gs {players = reverse (players gs), currentIx = newIdx}

      -- Burn on any 10 played
      when (any (\c -> rank c == R10) played) burnPile

      -- Burn on four-of-a-kind (ignore 8s)
      let discard = discardPile gs
          top4 = take 4 discard
          ranks = [r | Card r _ <- top4, r /= R8] -- Filter out transparent 8s
      when (length ranks == 4 && not (null ranks) && all (== head ranks) (tail ranks)) burnPile

      -- Three 3s forces next player to pick up (if extension active)
      when (ExtThree3s `elem` exts) $ do
        gsNow <- get
        let pile = discardPile gsNow
        when (length played == 3 && all (\c -> rank c == R3) played && not (null pile)) $ do
          let nextIdx = (currentIx gsNow + 1) `mod` length (players gsNow)
              nextPlayer = players gsNow !! nextIdx
          giveWastePileTo nextPlayer

      -- 9♣ steals a card from next player (if extension active)
      when (ExtNineClubs `elem` exts && any (\c -> rank c == R9 && suit c == Clubs) played) $ do
        gsNow <- get
        let curIdx = currentIx gsNow
            nextIdx = (curIdx + 1) `mod` length (players gsNow)
            currentP = players gsNow !! curIdx
            nextP = players gsNow !! nextIdx
        unless (null (hand nextP)) $ do
          let (idx, newGen) = randomR (0, length (hand nextP) - 1) (rng gsNow)
              stolenCard = hand nextP !! idx
          modify $ \gs ->
            let updatePlayer p
                  | pId p == pId currentP = p {hand = hand p ++ [stolenCard]}
                  | pId p == pId nextP = p {hand = delete stolenCard (hand p)}
                  | otherwise = p
             in gs {players = map updatePlayer (players gs), rng = newGen}

    burnPile = modify $ \gs ->
      -- Clear discard pile and add to burned piles
      gs {discardPile = [], burnedPiles = discardPile gs : burnedPiles gs}

    advanceToNextPlayer = modify $ \gs ->
      -- Move to next player clockwise
      gs {currentIx = (currentIx gs + 1) `mod` length (players gs)}

-- | Runs the game loop until only one player remains
gameLoop :: State GameState String
gameLoop = do
  gs <- get
  let activePlayers = filter (\p -> not (all null [hand p, faceUp p, faceDown p])) (players gs)

  if length activePlayers <= 1 -- Game over condition
    then return $ case activePlayers of
      [] -> "No winner"
      [winner] -> "Winner: " ++ pName winner
      _ -> "Error: multiple active players"
    else do
      applyStrategy -- Play one turn
      gameLoop -- Continue game

-- | Plays one complete game with 3 basic strategy players
playOneGame :: IO ()
playOneGame = do
  putStrLn "Starting Karma game with 3 players."
  gen <- newStdGen

  let fullDeck =
        shuffleDeck
          gen
          [Card r s | r <- [minBound .. maxBound], s <- [minBound .. maxBound]] -- Create full 52-card deck

      -- Deal 9 cards to each of 3 players: 3 hand, 3 face-up, 3 face-down
      (hand0, rest0) = splitAt 3 fullDeck
      (faceUp0, rest1) = splitAt 3 rest0
      (faceDown0, rest2) = splitAt 3 rest1
      (hand1, rest3) = splitAt 3 rest2
      (faceUp1, rest4) = splitAt 3 rest3
      (faceDown1, rest5) = splitAt 3 rest4
      (hand2, rest6) = splitAt 3 rest5
      (faceUp2, rest7) = splitAt 3 rest6
      (faceDown2, drawPile') = splitAt 3 rest7

      players' =
        [ Player 0 "Player1" hand0 faceUp0 faceDown0,
          Player 1 "Player2" hand1 faceUp1 faceDown1,
          Player 2 "Player3" hand2 faceUp2 faceDown2
        ]

      initialState =
        GameState
          { players = players',
            currentIx = 0,
            drawPile = drawPile',
            discardPile = [],
            burnedPiles = [],
            rng = gen,
            finishedOrder = [],
            activeExtensions = []
          }

  let (result, _) = runState (chooseStartingPlayer >> gameLoop) initialState
  putStrLn result

-- | Chooses starting player based on lowest card rank priority
chooseStartingPlayer :: State GameState ()
chooseStartingPlayer = do
  gs <- get
  let playersList = players gs

      getLowestHandCard player =
        if null (hand player)
          then Nothing
          else Just (minimumBy (comparing rank) (hand player))
      playersWithCards =
        [(p, card) | p <- playersList, Just card <- [getLowestHandCard p]]
  case playersWithCards of
    [] -> modify $ \gs -> gs {currentIx = 0} -- Default to first player
    playersWithCards' -> do
      let (bestPlayer, bestCard) = minimumBy (\(_, c1) (_, c2) -> comparing rank c1 c2) playersWithCards' -- Find lowest card overall
          playerIdx = fromJust $ elemIndex bestPlayer playersList

      modify $ \gs ->
        let idx = playerIdx
            removeCard p =
              if pId p == pId bestPlayer
                then p {hand = delete bestCard (hand p)}
                else p
            players' = map removeCard (players gs)
         in gs
              { currentIx = idx,
                players = players',
                discardPile = [bestCard]
              }
  where
    fromJust (Just x) = x
    fromJust Nothing = error "fromJust: Nothing"
    elemIndex x xs = case [i | (i, v) <- zip [0 ..] xs, pId v == pId x] of -- Find index by player ID
      [] -> Nothing
      (i : _) -> Just i

--------------------------------------------------------------------------------
-- STEP 3: ADVANCED BOT & HISTORY
--------------------------------------------------------------------------------

-- | Strategy that plays largest legal sets of same-rank cards
basicStrategySets :: State GameState Deck
basicStrategySets = do
  mplayer <- getCurrentPlayer
  gs <- get
  playMove mplayer gs
  where
    playMove Nothing _ = return []
    playMove (Just player) gs
      | not (null handLegalSets) = return (largestSet handLegalSets)
      | canPlayFaceUp player gs = return (largestSet faceUpLegalSets)
      | canPlayFaceDown player = pickRandomFaceDown player
      | otherwise = return []
      where
        top = listToMaybe (discardPile gs)
        handLegalSets = legalCardSets top (hand player)
        faceUpLegalSets = legalCardSets top (faceUp player)
        canPlayFaceUp p gs =
          null (hand p) && null (drawPile gs) && not (null faceUpLegalSets)
        canPlayFaceDown p =
          null (hand p) && null (faceUp p) && not (null (faceDown p))
        pickRandomFaceDown p = do
          let cards = faceDown p
              (idx, newGen) = randomR (0, length cards - 1) (rng gs)
          put $ gs {rng = newGen}
          return [cards !! idx]

    -- Helper groups legal cards by rank into sets
    legalCardSets :: Maybe Card -> Deck -> [Deck]
    legalCardSets top cards =
      let groups = groupBySameRank cards
       in filter (all (legalPlay top)) groups
    groupBySameRank :: Deck -> [Deck]
    groupBySameRank cards =
      groupBy (\c1 c2 -> rank c1 == rank c2) (sortBy (comparing rank) cards)
    largestSet :: [Deck] -> Deck
    largestSet [] = []
    largestSet sets = maximumBy (comparing length) sets

-- | Game loop that prints detailed turn-by-turn history
gameLoopWithHistory :: State GameState String
gameLoopWithHistory = do
  gs <- get
  let activePlayers = filter (\p -> not (all null [hand p, faceUp p, faceDown p])) (players gs)

  if length activePlayers <= 1 -- Game over
    then case activePlayers of
      [] -> return "\n=== FINAL STANDINGS ===\nNo winner\n"
      [winner] ->
        let finished = finishedOrder gs
            finishedStr =
              if null finished
                then ""
                else "\nElimination order:\n" ++ unlines [show pos ++ ". Player " ++ show pid | (pos, pid) <- zip [2 ..] (reverse finished)]
         in return $ "\n=== FINAL STANDINGS ===\nWINNER: " ++ pName winner ++ finishedStr
      _ -> return "\n=== FINAL STANDINGS ===\nError: multiple active players\n"
    else do
      gsNow <- get
      let curIx = currentIx gsNow
          player = players gsNow !! curIx
          beforeStr =
            -- Create turn header with player state
            "\n"
              ++ pName player
              ++ "'s turn:\n"
              ++ "  Hand: "
              ++ show (hand player)
              ++ "\n"
              ++ "  Face-up: "
              ++ show (faceUp player)
              ++ "\n"
              ++ "  Face-down: "
              ++ show (length (faceDown player))
              ++ " cards\n"
              ++ "  Discard: "
              ++ show (discardPile gsNow)
              ++ "\n"

      gsBefore <- get -- Snapshot state before turn
      let playerBefore = players gsBefore !! currentIx gsBefore
          burnedBefore = burnedPiles gsBefore
          pidBefore = pId playerBefore

      applyStrategyForPlayer
      gsAfter <- get -- Snapshot state after turn
      let burnedAfter = burnedPiles gsAfter
          burnMsg =
            if length burnedAfter > length burnedBefore
              then ">>> BURN: Discard pile cleared\n"
              else ""

          mPlayerAfter = find (\p -> pId p == pidBefore) (players gsAfter) -- Find same player after turn
          playerHadCards = not (all null [hand playerBefore, faceUp playerBefore, faceDown playerBefore])
          playerHasCards = case mPlayerAfter of
            Just p -> not (all null [hand p, faceUp p, faceDown p])
            Nothing -> False
          outMsg =
            if playerHadCards && not playerHasCards
              then ">>> " ++ pName playerBefore ++ " GETS OUT OF THE GAME\n"
              else ""

      rest <- gameLoopWithHistory
      return $ beforeStr ++ burnMsg ++ outMsg ++ rest -- Combine with current turn

-- | Runs one game with detailed history output
runOneGameWithHistory :: IO ()
runOneGameWithHistory = do
  gen <- newStdGen

  let fullDeck = shuffleDeck gen [Card r s | r <- [minBound .. maxBound], s <- [minBound .. maxBound]]

      -- Deal cards
      (hand0, rest0) = splitAt 3 fullDeck
      (faceUp0, rest1) = splitAt 3 rest0
      (faceDown0, rest2) = splitAt 3 rest1
      (hand1, rest3) = splitAt 3 rest2
      (faceUp1, rest4) = splitAt 3 rest3
      (faceDown1, rest5) = splitAt 3 rest4
      (hand2, rest6) = splitAt 3 rest5
      (faceUp2, rest7) = splitAt 3 rest6
      (faceDown2, drawPile') = splitAt 3 rest7

      players' =
        -- Mix of strategies for testing
        [ Player 0 "Basic1" hand0 faceUp0 faceDown0,
          Player 1 "SetBot" hand1 faceUp1 faceDown1,
          Player 2 "Basic2" hand2 faceUp2 faceDown2
        ]

      initialState =
        GameState
          { players = players',
            currentIx = 0,
            drawPile = drawPile',
            discardPile = [],
            burnedPiles = [],
            rng = gen,
            finishedOrder = [],
            activeExtensions = []
          }

  putStrLn "=== STARTING KARMA GAME WITH HISTORY ==="
  putStrLn "Players: Basic1 (basic strategy), SetBot (sets strategy), Basic2 (basic strategy)"

  let (result, _) =
        runState
          ( do
              chooseStartingPlayer
              gs <- get
              let starter = players gs !! currentIx gs
                  openingCard = case discardPile gs of
                    [] -> "(no card yet)"
                    (c : _) -> show c
              history <- gameLoopWithHistory
              return $
                "\nStarting player: "
                  ++ pName starter
                  ++ "\nOpening move: "
                  ++ openingCard
                  ++ "\n\n=== GAME BEGINS ===\n"
                  ++ history
          )
          initialState

  putStrLn result

--------------------------------------------------------------------------------
-- STEP 4: RULE EXTENSIONS
--------------------------------------------------------------------------------

-- | Plays one game with specified rule extensions
playOneGameStep4 :: [Extension] -> IO ()
playOneGameStep4 extensions = do
  gen <- newStdGen

  let fullDeck = shuffleDeck gen [Card r s | r <- [minBound .. maxBound], s <- [minBound .. maxBound]]

      -- Deal cards
      (hand0, rest0) = splitAt 3 fullDeck
      (faceUp0, rest1) = splitAt 3 rest0
      (faceDown0, rest2) = splitAt 3 rest1
      (hand1, rest3) = splitAt 3 rest2
      (faceUp1, rest4) = splitAt 3 rest3
      (faceDown1, rest5) = splitAt 3 rest4
      (hand2, rest6) = splitAt 3 rest5
      (faceUp2, rest7) = splitAt 3 rest6
      (faceDown2, drawPile') = splitAt 3 rest7

      players' =
        [ Player 0 "Basic1" hand0 faceUp0 faceDown0,
          Player 1 "SetBot" hand1 faceUp1 faceDown1,
          Player 2 "Basic2" hand2 faceUp2 faceDown2
        ]

      initialState =
        GameState
          { players = players',
            currentIx = 0,
            drawPile = drawPile',
            discardPile = [],
            burnedPiles = [],
            rng = gen,
            finishedOrder = [],
            activeExtensions = extensions -- Use extensions
          }

  putStrLn $ "=== KARMA WITH EXTENSIONS: " ++ show extensions ++ " ==="
  putStrLn "Players: Basic1 (basic), SetBot (sets), Basic2 (basic)"

  let (result, _) =
        runState
          ( do
              chooseStartingPlayer
              gs <- get
              let starter = players gs !! currentIx gs
                  openingCard = case discardPile gs of
                    [] -> "(no card yet)"
                    (c : _) -> show c
              history <- gameLoopWithHistory
              return $
                "\nStarting player: "
                  ++ pName starter
                  ++ "\nOpening move: "
                  ++ openingCard
                  ++ "\n\n=== GAME BEGINS ===\n"
                  ++ history
          )
          initialState

  putStrLn result

--------------------------------------------------------------------------------
-- STEP 5: SMART BOT & TOURNAMENTS
--------------------------------------------------------------------------------

-- Strategy tuning constants
largePileThreshold :: Int
largePileThreshold = 8 -- Burn 10s when pile has more than 8 cards

aggressiveHandSize :: Int
aggressiveHandSize = 5 -- Play aggressively when hand has >5 cards

-- | Smart strategy.
--
-- Prioritizes plays in this order:
--
-- 1. Complete four-of-a-kind burns
-- 2. Burn large piles (>8 cards) with 10s
-- 3. Play sets of 3+ cards
-- 4. Play any legal set
-- 5. Play adaptive single cards
--
-- Adapts based on hand size and pile size.
smartStrategy :: State GameState Deck
smartStrategy = do
  mplayer <- getCurrentPlayer
  gs <- get
  playMove mplayer gs
  where
    playMove Nothing _ = return []
    playMove (Just player) gs
      | not (null (hand player)) = playFromHand player gs
      | canPlayFaceUp player gs = playFromFaceUp player gs
      | canPlayFaceDown player = playFromFaceDown player gs
      | otherwise = return []

    canPlayFaceUp player gs =
      null (hand player) && null (drawPile gs) && not (null (faceUp player))

    canPlayFaceDown player =
      null (hand player) && null (faceUp player) && not (null (faceDown player))

    playFromHand player gs
      | not (null burnCards) = return burnCards -- Priority 1 -> complete four-of-a-kind burn
      | shouldBurn = return [head tens] -- Priority 2 -> burn large pile with 10
      | not (null bigSets) = return (maximumBy (comparing length) bigSets) -- Priority 3 -> play large sets (3+ cards)
      | not (null legalSets) = return (maximumBy (comparing length) legalSets) -- Priority 4 -> play any set
      | not (null handLegal) = return [selectAdaptiveCard handLegal player] -- Priority 5 -> adaptive single card
      | otherwise = return []
      where
        topCard = listToMaybe (discardPile gs)
        handLegal = validPlays topCard (hand player)
        legalSets = getLegalSets topCard handLegal
        pileSize = length (discardPile gs)
        tens = filter (\c -> rank c == R10) handLegal
        shouldBurn = pileSize > largePileThreshold && not (null tens) -- Burn if pile large and we have 10
        bigSets = filter (\s -> length s >= 3) legalSets -- Sets of 3+ cards
        burnCards = canCompleteBurn handLegal gs -- Check for four-of-a-kind completion

    -- Similar logic for face-up cards
    playFromFaceUp player gs
      | not (null burnCards) = return burnCards
      | shouldBurn = return [head tens]
      | not (null bigSets) = return (maximumBy (comparing length) bigSets)
      | not (null legalSets) = return (maximumBy (comparing length) legalSets)
      | not (null faceUpLegal) = return [minimumBy (comparing rank) faceUpLegal]
      | otherwise = return []
      where
        topCard = listToMaybe (discardPile gs)
        faceUpLegal = validPlays topCard (faceUp player)
        legalSets = getLegalSets topCard faceUpLegal
        pileSize = length (discardPile gs)
        tens = filter (\c -> rank c == R10) faceUpLegal
        shouldBurn = pileSize > largePileThreshold && not (null tens)
        bigSets = filter (\s -> length s >= 3) legalSets
        burnCards = canCompleteBurn faceUpLegal gs

    playFromFaceDown player gs -- Random face-down play
      | null cards = return []
      | otherwise = do
          put (gs {rng = newGen})
          return [cards !! idx]
      where
        cards = faceDown player
        (idx, newGen) = randomR (0, length cards - 1) (rng gs)

    -- Helper group legal cards by rank into sets
    getLegalSets topCard cards = filter (not . null) groupedSets
      where
        grouped = groupBy (\c1 c2 -> rank c1 == rank c2) (sortBy (comparing rank) cards)
        groupedSets = filter (all (legalPlay topCard)) grouped

    -- Helper adaptive card selection based on hand size
    selectAdaptiveCard cards player
      | handSize > aggressiveHandSize = maximumBy (comparing rank) cards -- Aggressive -> play high
      | otherwise = minimumBy (comparing rank) cards -- Conservative -> play low
      where
        handSize = length (hand player)

    -- Helper detect four-of-a-kind completion opportunity
    canCompleteBurn cards gs =
      let top3 = take 3 [c | c <- discardPile gs, rank c /= R8] -- Look at top 3 non 8 cards
       in if length top3 == 3 && all (\c -> rank c == rank (head top3)) top3 -- Check if all same rank
            then filter (\c -> rank c == rank (head top3)) cards -- Return matching cards from hand
            else [] -- No completion possible

-- | Applies strategy for current player based on their assigned strategy name
applyStrategyForPlayer :: State GameState ()
applyStrategyForPlayer = do
  gs <- get
  let player = players gs !! currentIx gs

  play <- routeStrategy player
  if null play
    then giveWastePileTo player
    else do
      let playerBefore = player -- Remember player before turn
      modify $ \gs ->
        let idx = currentIx gs
            remove zone = zone \\ play
            updatedPlayer -- Remove played cards from correct zone
              | play `subset` hand player = player {hand = remove (hand player)}
              | play `subset` faceUp player = player {faceUp = remove (faceUp player)}
              | otherwise = player {faceDown = remove (faceDown player)}
            players' = take idx (players gs) ++ [updatedPlayer] ++ drop (idx + 1) (players gs)
         in gs
              { players = players',
                discardPile = discardPile gs ++ play
              }

      processAfterPlayEffects play -- Handle special effects

      -- Track elimination (add to finishedOrder)
      gsAfter <- get
      let mPlayerAfter = find (\p -> pId p == pId playerBefore) (players gsAfter)
          wasActive = not (all null [hand playerBefore, faceUp playerBefore, faceDown playerBefore])
          nowFinished = case mPlayerAfter of
            Just p -> all null [hand p, faceUp p, faceDown p] -- Player now has no cards
            Nothing -> False

      when (wasActive && nowFinished) $ -- If player was eliminated this turn
        modify $
          \gs -> gs {finishedOrder = finishedOrder gs ++ [pId playerBefore]}

  replenishCards player
  advanceToNextPlayer
  where
    subset xs ys = all (`elem` ys) xs

    routeStrategy player = case pName player of -- Route based on player name
      "SmartBot" -> smartStrategy
      "SetBot" -> basicStrategySets
      _ -> basicStrategy
    processAfterPlayEffects :: Deck -> State GameState ()
    processAfterPlayEffects played = do
      gs <- get
      let exts = activeExtensions gs

      when (ExtReverse8 `elem` exts && any (\c -> rank c == R8) played) $ modify $ \gs ->
        let n = length (players gs)
            oldIdx = currentIx gs
            newIdx = n - 1 - oldIdx
         in gs {players = reverse (players gs), currentIx = newIdx}

      when (any (\c -> rank c == R10) played) burnPile

      let discard = discardPile gs
          top4 = take 4 discard
          ranks = [r | Card r _ <- top4, r /= R8]
      when (length ranks == 4 && not (null ranks) && all (== head ranks) (tail ranks)) burnPile

      when (ExtThree3s `elem` exts) $ do
        gsNow <- get
        let pile = discardPile gsNow
        when (length played == 3 && all (\c -> rank c == R3) played && not (null pile)) $ do
          let nextIdx = (currentIx gsNow + 1) `mod` length (players gsNow)
              nextPlayer = players gsNow !! nextIdx
          giveWastePileTo nextPlayer

      when (ExtNineClubs `elem` exts && any (\c -> rank c == R9 && suit c == Clubs) played) $ do
        gsNow <- get
        let curIdx = currentIx gsNow
            nextIdx = (curIdx + 1) `mod` length (players gsNow)
            currentP = players gsNow !! curIdx
            nextP = players gsNow !! nextIdx
        unless (null (hand nextP)) $ do
          let (idx, newGen) = randomR (0, length (hand nextP) - 1) (rng gsNow)
              stolenCard = hand nextP !! idx
          modify $ \gs ->
            let updatePlayer p
                  | pId p == pId currentP = p {hand = hand p ++ [stolenCard]}
                  | pId p == pId nextP = p {hand = delete stolenCard (hand p)}
                  | otherwise = p
             in gs {players = map updatePlayer (players gs), rng = newGen}

    burnPile = modify $ \gs ->
      gs {discardPile = [], burnedPiles = discardPile gs : burnedPiles gs}

    advanceToNextPlayer = modify $ \gs ->
      gs {currentIx = (currentIx gs + 1) `mod` length (players gs)}

-- | Game loop with turn limit to prevent infinite loops
gameLoopForTournament :: State GameState String
gameLoopForTournament = gameLoopHelper 0
  where
    maxTurns = 1000 -- Prevent infinite turns
    gameLoopHelper turnCount
      | turnCount >= maxTurns = return "Game timeout" -- Timeout after max turns
      | otherwise = do
          gs <- get
          let activePlayers = filter (\p -> not (all null [hand p, faceUp p, faceDown p])) (players gs)

          if length activePlayers <= 1 -- Game over
            then return $ case activePlayers of
              [] -> "No winner"
              [winner] -> pName winner -- Return winner name only
              _ -> "Error: multiple active"
            else do
              applyStrategyForPlayer
              gameLoopHelper (turnCount + 1)

-- | Plays one tournament game silently and returns winner name
playOneTournamentGame :: IO String
playOneTournamentGame = do
  gen <- newStdGen

  let fullDeck = shuffleDeck gen [Card r s | r <- [minBound .. maxBound], s <- [minBound .. maxBound]]

      -- Deal cards
      (hand0, rest0) = splitAt 3 fullDeck
      (faceUp0, rest1) = splitAt 3 rest0
      (faceDown0, rest2) = splitAt 3 rest1
      (hand1, rest3) = splitAt 3 rest2
      (faceUp1, rest4) = splitAt 3 rest3
      (faceDown1, rest5) = splitAt 3 rest4
      (hand2, rest6) = splitAt 3 rest5
      (faceUp2, rest7) = splitAt 3 rest6
      (faceDown2, drawPile') = splitAt 3 rest7

      players' =
        [ Player 0 "Basic1" hand0 faceUp0 faceDown0,
          Player 1 "SetBot" hand1 faceUp1 faceDown1,
          Player 2 "SmartBot" hand2 faceUp2 faceDown2
        ]

      initialState =
        GameState
          { players = players',
            currentIx = 0,
            drawPile = drawPile',
            discardPile = [],
            burnedPiles = [],
            rng = gen,
            finishedOrder = [],
            activeExtensions = []
          }

  let (winner, _) = runState (chooseStartingPlayer >> gameLoopForTournament) initialState
  return winner

-- | Runs n tournament games and returns win counts
playTournament :: Int -> IO [(String, Int)]
playTournament n = do
  putStrLn $ "Running tournament with " ++ show n ++ " games..."

  winners <- playNGamesLoop n [] -- Play n games
  let countWins name = length (filter (== name) winners) -- Count wins per strategy
      results =
        [ ("Basic1", countWins "Basic1"),
          ("SetBot", countWins "SetBot"),
          ("SmartBot", countWins "SmartBot")
        ]

  putStrLn "\n=== TOURNAMENT RESULTS ==="
  printResults results
  return results
  where
    playNGamesLoop 0 acc = return (reverse acc) -- return winners
    playNGamesLoop remaining acc = do
      winner <- playOneTournamentGame -- Play one game
      when (remaining `mod` 100 == 0) $ putStrLn $ "Games remaining: " ++ show remaining
      playNGamesLoop (remaining - 1) (winner : acc)
    printResults [] = return ()
    printResults ((name, wins) : rest) = do
      putStrLn $ name ++ ": " ++ show wins ++ " wins (" ++ show (100 * wins `div` n) ++ "%)" -- Print with percentage
      printResults rest
