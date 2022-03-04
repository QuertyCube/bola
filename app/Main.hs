{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Use guards" #-}

import Control.Arrow (Arrow (second))
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (State, StateT, evalStateT, execState, get, modify, put, runState)
import Control.Monad.Trans.Writer (WriterT, execWriterT, tell)
import Debug.Trace
import GHC.Base (Float)
import GHC.Num (Num (fromInteger))
import GHC.Real (fromIntegral)
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import System.Directory
import System.IO.Error hiding (catch)
import System.IO.Unsafe
import System.Random
import Prelude hiding (catch)

main :: IO ()
main = do
  removeIfExists "log.txt"
  putStrLn "velocity (kecepatan bola bergerak): "
  velInput <- getLine
  putStrLn "max velo (kecepatan maksimum bola): "
  maxVelInput <- getLine
  putStrLn "size frame (width): "
  widthInput <- getLine
  putStrLn "size frame (height): "
  heightInput <- getLine
  putStrLn "Penambahan kecepatan yg akan ditambahkan saaat bola memantul: "
  timeInput <- getLine

  let width = (read widthInput :: Int)
      height = (read heightInput :: Int)
      randWidth = fromIntegral (unsafePerformIO (randomN 1 (width - 20))) :: Float
      randHeight = fromIntegral (unsafePerformIO (randomN 1 (height - 20))) :: Float
      maxCepat = (read maxVelInput :: Float) + 7
   in simulate
        (InWindow "Bounce Ball" (width, height) (50, 50))
        white
        30
        BallMod {position = (randWidth, randHeight), angle = ((read velInput :: Float) + 7, 3)}
        (toPict width height)
        (iteration width height (read timeInput :: Float) maxCepat)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

toPict :: Int -> Int -> BallMod -> Picture
toPict a b BallMod {position = (px, py)} =
  translate (-(fromIntegral a * 0.5)) (-(fromIntegral b * 0.5)) $
    translate px py $ circleSolid 20

data BallMod = BallMod {position :: Point, angle :: Point}

randomN :: Int -> Int -> IO Int
randomN strt end = getStdRandom (randomR (strt, fromIntegral end))

toInt :: Float -> Int
toInt = round

iteration :: Int -> Int -> Float -> Float -> ViewPort -> Float -> BallMod -> BallMod
iteration a b tambah akhir _ _ BallMod {position = (px, py), angle = (vx, vy)} =
  let vx' =
        if (((px + vx) + 20) >= fromIntegral a)
          then
            if vx <= akhir
              then unsafePerformIO $ do
                cetak px py

                return ((-vx) * ((10 + tambah) * 0.1))
              else unsafePerformIO $ do
                cetak px py
                return (-vx)
          else
            if ((px + vx) - 20) <= 0
              then
                if vx >= (-akhir)
                  then unsafePerformIO $ do
                    cetak px py
                    return ((-vx) * ((10 + tambah) * 0.1))
                  else unsafePerformIO $ do
                    cetak px py
                    return (-vx)
              else vx

      vy' =
        if ((py + vy) + 20) >= fromIntegral b
          then
            if vx >= (-akhir) && vx <= akhir
              then unsafePerformIO $ do
                cetak px py
                return ((-vy) * ((10 + tambah) * 0.1))
              else unsafePerformIO $ do
                cetak px py
                return (-vy)
          else
            if ((py + vy) - 20) <= 0
              then
                if vx >= (-akhir) && vx <= akhir
                  then unsafePerformIO $ do
                    cetak px py
                    return ((-vy) * ((10 + tambah) * 0.1))
                  else unsafePerformIO $ do
                    cetak px py

                    return (-vy)
              else
                if ((px + vx) + 20) >= fromIntegral a
                  then
                    if vx <= akhir
                      then vy * ((10 + tambah) * 0.1) -- +(-10)
                      else vy
                  else
                    if ((px + vx) - 20) <= 0
                      then
                        if vx >= (-akhir)
                          then vy * ((10 + tambah) * 0.1)
                          else vy
                      else vy
   in BallMod {position = (px + vx, py + vy), angle = (vx', vy')}

newtype Stack a = Stack {unStack :: StateT Int (WriterT [Float] IO) a}

foo :: Float -> Float -> Stack ()
foo x y = Stack $ do
  lift $ tell [x]
  lift $ tell [y]
  return ()

evalStack :: Stack a -> IO [Float]
evalStack m = execWriterT (evalStateT (unStack m) 0)

cetak :: Float -> Float -> IO ()
cetak b c = do
  x <- evalStack (foo b c)
  print x
  appendFile "log.txt" (show x ++ "\n")

-- Animation Task (harus selesai tanggal 2 Maret)

-- 1.Bola harus memantul di dalam frame window 👌

-- 2.Kecepatan bola di awal aplikasi berjalan pelan (ditentukan dari value argument yg diinput user)

-- 3.Setiap kali bola memantul akan terjadi sbb :
--  a. Arah dan sudutnya berubah
--  b. Kecepatan meningkat setelah memantul (berdasarkan dari value argument yg diinput user)
--  c. Jika sudah mencapai kecepatan maksimum, maka kecepatan bola tidak akan berubah lagi,
--     walaupun memantul (maksimum kecepatan ditentukan dari value argument yg diinput user)

-- 4.Posisi bola diawal ditentukan dari posisi Random (x,y)

-- 5.input user (argument) yg harus diminta input dari user di awal aplikasi berjalan
-- a. velocity (kecepatan bola bergerak)
-- b. max velo (kecepatan maksimum bola)
-- c. size frame (width, height)
-- d. Penambahan kecepatan yg akan ditambahkan saaat bola memantul
