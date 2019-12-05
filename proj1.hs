
--Implement your solution here
--Remember to put function declarations as well

module Proj1 (feedback, initialGuess , nextGuess, GameState) where
-- Author: Lachlan Piper, login id: piperl
-- Purpose: Play card game (defined below) against a human opponent
--
-- This file contains a working solution to the first project of University
-- of Melbourne Subject COMP30020, Declarative Programming
--
-- The entire program is contained within this file, however it does not work
-- on its own. This file is only interacted with through the file Proj1Test.hs
-- and has not been made to function independently.
--
-- This program plays a card game with rules as follows:
    -- Two players face each other, each with a complete standard deck of
    -- western playing cards (without jokers). One player will be the answerer
    -- (user) and the other is the guesser (this program). The answerer begins
    -- by selecting some number of cards from his or her deck without showing
    -- the guesser. These cards will form the answer for this game. The aim of
    -- the game is for the guesser to guess the answer.
    --
    -- Once the answerer has selected the answer (specified on command line of
    -- Proj1Test.hs), the guesser chooses the same number of cards from their
    -- deck to form the guess and shows them to the answerer. The answerer
    -- responds by telling the guesser these five numbers as feedback for the
    -- guess:
    --    1. How many of the cards in the answer are also in the guess
    --      (correct cards).
    --
    --    2. How many cards in the answer have rank lower than the lowest
    --      rank in the guess (lower ranks). Ranks, in order from low to high,
    --      are 2–10, Jack, Queen, King, and Ace.
    --
    --    3. How many of the cards in the answer have the same rank as a
    --      card in the guess (correct ranks). For this, each card in the
    --      guess is only counted once. That is, if the answer has two queens
    --      and the guess has one, the correct ranks number would be 1, not 2.
    --      Likewise if there is one queen in the answer and two in the guess.
    --
    --    4. How many cards in the answer have rank higher than the highest
    --      rank in the guess (higher ranks).
    --
    --    5. How many of the cards in the answer have the same suit as a
    --      card in the guess, only counting a card in the guess once
    --      (correct suits). For example, if the answer has two clubs and the
    --      guess has one club, or vice versa, the correct suits number would
    --      be 1, not 2.
    --
    -- Note that the order of the cards in the answer and the guess is
    -- immaterial, and that, since they come from a single deck, cards cannot
    -- be repeated in either answer or guess.
    --
    -- The guesser then guesses again, and receives feedback for the new
    -- guess, repeating the process until the guesser guesses the answer
    -- correctly. The object of the game for the guesser is to guess the
    -- answer with the fewest possible guesses.
--
-- Method of guessing the answer:
    -- The program contains a GameState, containing all possible correct
    -- guesses of the answer. Depending on the number of cards in the
    -- answer, the program makes an 'educated' initial guess. Proj1Test.hs
    -- submits this for feedback, and passes the gamestate, previous guess
    -- and feedback back to this program to make the next guess. The
    -- program then modifies the gamestate to contain all new possible correct
    -- guesses given this information, and returns a new guess and the new
    -- gamestate. Proj1Test.hs takes these, applies the new guess and returns
    -- feedback data to the program to make its next guess. This last step
    -- repeats until the guess this program outputs == the answer.

import Card
import Data.List
-- Gamestate holds a list of all possible correct guesses of the answer.
-- Initially, it holds all possible combinations of cards (without repeats)
type GameState = [[Card]]

-- initalGuess takes the number of cards the guess should be made of
-- and returns an initial guess, as well as the initial gamestate, in a tuple
initialGuess:: Int -> ([Card],GameState)
initialGuess x = (initialCards, (createGameState x allCards ))
    where
      initialCards = [Card (toEnum x :: Suit) (toEnum y :: Rank) |(y,x)<- combo]
      guessRanks = take x [(ratio`div`2), (1+(ratio`div`2) + ratio) ..12]
      ratio = 13`div`(x + 1)
      -- all the suits are repeated for 4 or 5 sized guesses
      suits = [0..3]++[0..3]
      -- create (rank, suit) tuples for cards we will have in guess
      combo = zip guessRanks suits
      allCards = ([minBound..maxBound] :: [Card])

-- this will create all possible combinations of cards without repetition of Cards.
-- The order of this list is from smallest combination to largest
createGameState:: Int -> [Card]-> [[Card]]
createGameState 0 _ = [[]]
createGameState _ [] = []
createGameState guessSize (x:xs)
  | guessSize == 0 = []
  | otherwise = [x:y| y <- createGameState (guessSize -1) xs]
  ++ (createGameState guessSize xs)


-- takes the previous guess, gamestate and feedback to that guess,
-- returns an educated next guess, along with a new gamstate with all
-- possible bad guesses eliminated
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
-- For next guess, pick the middle value of the filtered gamestate.
-- It was found that choosing the middle value, on average, reduced the
-- number of guesses
nextGuess (gus, state) currFeedback =
          (newGamestate !! ((length newGamestate)`div`2), newGamestate)
  where
    -- change gamestate to only have guesses which would output the same
    -- feedback as the previous guess
    newGamestate = [x |x<-state, (feedback x gus=) == currFeedback]


-- takes an answer and a guess (as a list of cards), and returns a pentuple
-- dictating the feeback of that guess. The Pentuple represents
-- (#correct cards in guess, #lower ranks than in guess,
-- #correct ranks in guess, #higher ranks than in guess, #correct suits).
-- The pentuple is described in more detail at the top of the file
feedback::[Card] -> [Card] ->(Int,Int,Int,Int,Int)
feedback ans gus = (findCorrect ans gus,
    findLess ans gus,
    findEqualRank ans gus,
    findHigher ans gus,
    findSuits ans gus)

-- below are helper functions for the feedback function --
----------------------------------------------------------

-- takes an answer and guess, represented as list of cards, and returns the
-- number of matching cards in the guess
findCorrect::[Card] -> [Card] -> Int
findCorrect as gs = (length as) - (length(as\\gs))

-- takes an answer and guess, represented as list of cards, and returns the
-- number of cards in the answer which are lower than the smallest number in
-- the guess
findLess::[Card] -> [Card] -> Int
findLess as gs = length([x |Card _ x <- as, x < answerMin])
  where
    answerMin = minimum([z | Card _ z <- gs])

-- takes an answer and guess, represented as list of cards, and returns the
-- number of cards in the answer which have the same rank. For this, each card
-- in the guess is only counted once. That is, if the answer has two queens and
--the guess has one, the correct ranks number would be 1, not 2. Likewise if
--there is one queen in the answer and two in the guess.
findEqualRank::[Card] -> [Card] -> Int
findEqualRank as gs = (length as) - (length(answer \\ guess))
  where answer = [x | Card _ x <- as]
        guess = [x | Card _ x <- gs]

-- takes an answer and guess, represented as list of cards, and returns the
-- number of cards in the answer which are higher than the highest number in
-- the guess
findHigher::[Card] -> [Card] -> Int
findHigher as gs = length([x |Card _ x <- as, x > answerMax])
  where
    answerMax = maximum ([z | Card _ z <- gs])

-- takes an answer and guess, represented as list of cards, and returns the
-- number of cards in the answer which have the same Suit. only counting a card
--in the guess once (correct suits). For example, if the answer has two clubs
--and the guess has one club, or vice versa, the correct suits number would be
--1, not 2.
findSuits::[Card] -> [Card] -> Int
findSuits as gs = (length as) - (length(answer \\ guess))
  where answer = [x | Card x _ <- as]
        guess = [x | Card x _ <- gs]
