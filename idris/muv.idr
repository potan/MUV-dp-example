

data Application : (model:Type) -> (msg: model -> Type) -> (vtype : Type -> Type) -> Type where
  MUV : model -> (updater : (m:model) -> (msg m) -> model) -> (view : (m:model) -> vtype (msg m)) -> Application model msg vtype

interface View (c : Type -> Type) (view : Type -> Type) where
  runViewIn : (c (view msg)) -> (c msg)
  runView : (view msg) -> (c msg)

muvRun : (Monad m, View m v) => (Application modelType msgType v) -> m modelType
muvRun (MUV model updater view) =
  do
    msg <- runView (view model)
    muvRun (MUV (updater model msg) updater view)

-- Sample

data Model = Logouted | Logined String

data MsgOuted = Login String
data MsgIned  = Logout | Greet

msgType : Model -> Type
msgType Logouted = MsgOuted
msgType (Logined _) = MsgIned

View IO IO where
  runViewIn iov = iov >>= id
  runView = id

loginPage : IO MsgOuted
loginPage = do
           putStr "Login: "
           map Login getLine

genMsg : String -> MsgIned
genMsg "" = Logout
genMsg _ = Greet

workPage : String -> IO MsgIned
workPage name = do
           putStr ("Hello, " ++ name ++ "\n")
           putStr "Input empty string for logout or nonempty for greeting\n"
           map genMsg getLine

view : (m: Model) -> IO (msgType m)
view Logouted = loginPage
view (Logined name) = workPage name

updater : (m:Model) -> (msgType m) -> Model
updater Logouted (Login name) = Logined name
updater (Logined name) Logout = Logouted
updater (Logined name) Greet = Logined name

app : Application Model Main.msgType IO
app = MUV Logouted updater view

main : IO ()
main = do
  m <- muvRun app
  pure ()

lapp : a -> a
lapp x = x where
     mt : Model -> Type
     mt Logouted = MsgOuted
     mt (Logined _) = MsgIned

     v : (m: Model) -> IO (mt m)
     v Logouted = loginPage
     v (Logined name) = workPage name
 
     u : (m:Model) -> (mt m) -> Model
     u Logouted (Login name) = Logined name
     u (Logined name) Logout = Logouted
     u (Logined name) Greet = Logined name

     xapp : Application Model mt IO
     xapp = MUV Logouted u v
