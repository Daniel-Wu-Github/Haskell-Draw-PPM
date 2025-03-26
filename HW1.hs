import System.IO
import Text.Printf


{-Enter your name Daniel Wu
  Enter the date 2/7/2025
  (by entering your name and date you certify this is your
  own work and not that of any other person or service.
  Comment your functions for clarity.-}

{-Here the image size is defined as a 600 x 600 pixel image-}
-- Define image size
mySize :: Int
mySize = 600

{-Here I have defined 2 rgb colors for a given pixel. -}
-- Define colors  
whiteR, whiteG, whiteB :: Int
whiteR = 255
whiteG = 255
whiteB = 255

maroonR, maroonG, maroonB :: Int
maroonR = 88
maroonG = 0
maroonB = 0

{-define other colors here, or create them as you move, like gradients-}

magentaR, magentaG, magentaB :: Int
magentaR = 213
magentaG = 91
magentaB = 232

generateImage :: [[(Int, Int, Int)]]
generateImage = [[pixelColor y x | x <- [0..mySize-1]] | y <- [0..mySize-1]]
   where
       mySize = 600 -- Define image size

       -- Colors
       white  = (255, 255, 255)
       yellow = (240, 240, 0)
       black  = (0, 0, 0)
       red   = (255, 60, 60)
       blue  = (60, 60, 255)
       green = (60, 255, 60)
       
       -- Determine pixel color
       pixelColor y x
           -- place ears with small rectangles
           | (y >= 175) && (y <= 200) && (x >= 255) && (x <= 265) = black -- left ear
           | (y >= 175) && (y <= 200) && (x >= 315) && (x <= 325) = black -- right ear
           -- flatten out the top by placing a rectangle
           | (y >= 170) && (y <= 200) && (x >= 130) && (x <= 450) = yellow
           -- Top-left carving circles
           | ((y - 210) ^ 2 + (x - 160) ^ 2 <= 2000) = yellow -- draw a circle at x = 160, y = 210 with radius 2000
           | ((y - 210) ^ 2 + (x - 210) ^ 2 <= 2000) = yellow -- draw a circle at x = 210, y = 210 with radius 2000
           -- Top-right carving circles
           | ((y - 210) ^ 2 + (x - 370) ^ 2 <= 2000) = yellow -- draw a circle at x = 370, y = 210 with radius 2000
           | ((y - 210) ^ 2 + (x - 420) ^ 2 <= 2000) = yellow -- draw a circle at x = 420, y = 210 with radius 2000
           -- Bottom-left carving circle
           | ((y - 400) ^ 2 + (x - 180) ^ 2 <= 2500) = yellow 
           -- Bottom-right carving circle
           | ((y - 400) ^ 2 + (x - 400) ^ 2 <= 2500) = yellow
           -- Inner black oval (foreground)
           | (4 * (y - 300) ^ 2) + (x - 300) ^ 2 <= 63000 = black
           -- Background yellow oval (largest)
           | (4 * (y - 300) ^ 2) + (x - 300) ^ 2 <= 102000 = yellow
           -- large triangle of white at the bottom
           | y >= 300 && y <= 600 && x >= 0 && x <= 600 && x >= (300 - (y - 300)) && x <= (300 + (y - 300)) = green
           -- red and blue background split right down the middle
           | x < 300 = red
           | x >= 300 = blue
           -- Default background color
           | otherwise = white




{-drawCircle :: (Int, Int) -> Int -> [[(Int, Int, Int)]]
drawCircle (centerX, centerY) radius = [[pixelColor x y | x <- [0..mySize-1]] | y <- [0..mySize-1]]
   where
    pixelColor x y = if (x - centerX) ^ 2 + (y - centerY) ^ 2 <= radius ^ 2
                     then (magentaR, magentaG, magentaB)
                     else (0, 0, 0)-}
     


-- Write the PPM file
writePPM :: FilePath -> [[(Int, Int, Int)]] -> IO ()
writePPM filename pixels = do
    let header = "P3\n" ++ show mySize ++ " " ++ show mySize ++ "\n255\n"
        body = unlines [unwords [printf "%d %d %d" r g b | (r, g, b) <- row] | row <- pixels]
    writeFile filename (header ++ body)

main :: IO ()
main = do
    let image = generateImage
    writePPM "Batman.ppm" image
    putStrLn "PPM file 'Batman.ppm' created successfully."
    