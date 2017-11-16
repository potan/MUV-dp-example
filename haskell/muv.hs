
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}

import System.IO

type View a = IO a

data Model = forall m. (Updatable m, Viewable m) => Model m

class Updatable m where
  data Message m :: *
  update :: m -> (Message m) -> Model

class (Updatable m) => Viewable m where
  view :: m -> (View (Message m))

data Logouted = Logouted

data Logined = Logined String

instance Updatable Logouted where
  data Message Logouted = Login String
  update Logouted (Login name) = Model (Logined name)

instance Updatable Logined where
  data Message Logined = Logout | Greeting
  update m Logout = Model Logouted
  update m Greeting = Model m

instance Viewable Logouted where
  view Logouted = do
      putStr "Login: "
      hFlush stdout
      fmap Login getLine

instance Viewable Logined where
  view (Logined name) = do
      putStr $ "Hello " ++ name ++ "!\n"
      hFlush stdout
      l <- getLine
      pure $ if l == ""
             then
               Logout
             else
               Greeting

runMUV :: Model -> IO a
runMUV (Model m) = do
  msg <- view m
  runMUV $ update m msg

main :: IO ()
main = runMUV (Model Logouted)
