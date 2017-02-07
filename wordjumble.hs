{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Char
import           System.Random
import           Graphics.Blank

main :: IO ()
main = blankCanvas 3000 { events = ["mousedown"] } $ \ context -> do
--fetch jumbled letters from 'getWords' function and place in characters
                                                                  got <- getWords
                                                                  let one = got !! 0
                                                                  let two = got !! 1
                                                                  let three = got !! 2
                                                                  let four = got !! 3
                                                                  let five = got !! 4
                                                                  let (w,h)= (width context, height context)
                                                                  let sz = min w h
--print jumbled letters to screen
                                                                  send context $ do
                                                                              sequence_ [ do save()
                                                                                             translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                                                                                             font "120pt Calibri"
                                                                                             fillStyle "blue"
                                                                                             --function ltt converts character to text to print
                                                                                             fillText(ltt one, -0.33 * sz, 0.8 * sz)
                                                                                             restore()
                                                                                             | x <- [2]
                                                                                             , y <- [-2]
                                                                                             ]
                                                                              restore()

                                                                              sequence_ [ do save()
                                                                                             translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                                                                                             font "120pt Calibri"
                                                                                             fillStyle "blue"
                                                                                             fillText(ltt two, -0.33 * sz, 0.8 * sz)
                                                                                             restore()
                                                                                             | x <- [3]
                                                                                             , y <- [-2]
                                                                                             ]
                                                                              restore()

                                                                              sequence_ [ do save()
                                                                                             translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                                                                                             font "120pt Calibri"
                                                                                             fillStyle "blue"
                                                                                             fillText(ltt three, -0.33 * sz, 0.8 * sz)
                                                                                             restore()
                                                                                             | x <- [4]
                                                                                             , y <- [-2]
                                                                                             ]
                                                                              restore()

                                                                              sequence_ [ do  save()
                                                                                              translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                                                                                              font "120pt Calibri"
                                                                                              fillStyle "blue"
                                                                                              fillText(ltt four, -0.33 * sz, 0.8 * sz)
                                                                                              restore()
                                                                                              | x <- [5]
                                                                                              , y <- [-2]
                                                                                              ]
                                                                              restore()

                                                                              sequence_ [ do  save()
                                                                                              translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                                                                                              font "120pt Calibri"
                                                                                              fillStyle "blue"
                                                                                              fillText(ltt five, -0.33 * sz, 0.8 * sz)
                                                                                              restore()
                                                                                              | x <- [6]
                                                                                              , y <- [-2]
                                                                                              ]
                                                                              restore()
                                                                  loop context got Map.empty A

--data type for swapping letter draw control
data ABCDE = A | B | C | D | E
        deriving (Eq,Ord,Show)

--function to swap letter draw control
swap :: ABCDE -> ABCDE
swap A = B
swap B = C
swap C = D
swap D = E
swap E = A

--function to convert letter to text
ltt :: Char -> Text
ltt 'a' = "a"
ltt 'b' = "b"
ltt 'c' = "c"
ltt 'd' = "d"
ltt 'e' = "e"
ltt 'f' = "f"
ltt 'g' = "g"
ltt 'h' = "h"
ltt 'i' = "i"
ltt 'j' = "j"
ltt 'k' = "k"
ltt 'l' = "l"
ltt 'm' = "m"
ltt 'n' = "n"
ltt 'o' = "o"
ltt 'p' = "p"
ltt 'q' = "q"
ltt 'r' = "r"
ltt 's' = "s"
ltt 't' = "t"
ltt 'u' = "u"
ltt 'v' = "v"
ltt 'w' = "w"
ltt 'x' = "x"
ltt 'y' = "y"
ltt 'z' = "z"

--loop sends co-ordinates to draw the letters and grid
loop :: DeviceContext ->String-> Map (Int, Int) ABCDE -> ABCDE -> IO ()
loop context got board turn = do
--        print board
--        print turn
        let one = got !! 0
        let two = got !! 1
        let three = got !! 2
        let four = got !! 3
        let five = got !! 4
        (w,h,sz) <- send context $ do
                let (w,h) = (width context, height context)
                beginPath()
                let sz = min w h
                save()
                translate (w / 2, h / 2)
                --sends co-ordinates for the grid
                sequence_ [ do bigLine (-sz * 0.75,n) (sz * 0.75,n)
                               bigLine (n,-sz * 0.15) (n,sz * 0.15)
                               bigLine (m,-sz * 0.15) (m,sz * 0.15)
                               bigLine (l,-sz * 0.15) (l,sz * 0.15)
                          | n <- [-sz * 0.15,sz * 0.15]
                          , m <- [-sz * 0.45,sz * 0.45]
                          , l <- [-sz * 0.75,sz * 0.75]
                          ]
                --sends co-ordinates for drawing letters
                sequence_ [ do save()
                               translate (fromIntegral x * sz * 0.3,fromIntegral y * sz * 0.3)
                               case Map.lookup (x,y) board of
                                  Just A -> drawA one (sz * 0.1)
                                  Just B -> drawB two (sz * 0.1)
                                  Just C -> drawC three (sz * 0.1)
                                  Just D -> drawD four (sz * 0.1)
                                  Just E -> drawE five (sz * 0.1)
                                  Nothing -> return ()
                               restore()
                          | x <- [-2,-1,0,1,2]
                          , y <- [0]
                          ]
                restore()
                return (w,h,sz)
        --pointing to cell
        let pointToSq :: (Double, Double) -> Maybe (Int,Int)
            pointToSq (x,y) = do
                    x' <- fd ((x - w / 2) / sz)
                    y' <- fd ((y - h / 2) / sz)
                    return (x',y')

            fd x =
--                    trace (show ("fx",x,r)) $
                    if r `elem` [-2..2] then Just (r) else Nothing
                where r = round (x * 3.3333)

        event <- wait context
--        print event
        case ePageXY event of
           -- if no mouse location, ignore, and redraw
           Nothing -> loop context got board turn
           Just (x',y') -> case pointToSq (x',y') of
                             Nothing -> loop context got board turn
                             Just pos -> case Map.lookup pos board of
                                           Nothing -> loop context got
                                                            (Map.insert pos turn board)
                                                            (swap turn)
                                                    -- already something here
                                           Just _ -> loop context got board turn
boardColor = "black"

--drawing the letters
drawA :: Char->Double -> Canvas ()
drawA one size = do
        font "120pt Calibri"
        fillStyle "purple"
        fillText(ltt one, -0.33 * size, 0.8 * size)

drawB :: Char->Double -> Canvas ()
drawB two size = do
        font "120pt Calibri"
        fillStyle "purple"
        fillText(ltt two, -0.33 * size, 0.8 * size)

drawC :: Char->Double -> Canvas ()
drawC three size = do
        font "120pt Calibri"
        fillStyle "purple"
        fillText(ltt three, -0.33 * size, 0.8 * size)

drawD :: Char->Double -> Canvas ()
drawD four size = do
        font "120pt Calibri"
        fillStyle "purple"
        fillText(ltt four, -0.33 * size, 0.8 * size)

drawE :: Char->Double -> Canvas ()
drawE five size = do
        font "120pt Calibri"
        fillStyle "purple"
        fillText(ltt five, -0.33 * size, 0.8 * size)

--drawing the grid
bigLine :: (Double, Double) -> (Double, Double) -> Canvas ()
bigLine (x,y) (x',y') = do
        beginPath()
        moveTo(x,y)
        lineTo(x',y')
        lineWidth 20
        strokeStyle boardColor
        lineCap "round"
        stroke()

--shuffes the letters of the selected word
shuffle :: [t] -> IO [t]
shuffle [] = return []
shuffle xs = do
                randomPosition <- getStdRandom (randomR (0, length xs - 1))
                let (left, (a:right)) = splitAt randomPosition xs
                fmap (a:) (shuffle (left ++ right))

--function to randomly select a word from the file, pass it to shuffle function and get the shuffled letters
getWords :: IO [Char]
getWords = do
        contents <- readFile "words.txt"
        gen <- getStdGen
        let listWords = words contents
        let len = length listWords - 1
        let (listIndex, newGen) = randomR (0,len) gen :: (Int, StdGen)
        let selectedWord = listWords !! listIndex
        xs <- shuffle selectedWord
        return xs
