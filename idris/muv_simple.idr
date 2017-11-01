

data Application : (model:Type) -> (msg: model -> Type) -> (vtype : Type -> Type) -> Type where
  MUV : model -> (updater : (m:model) -> (msg m) -> model) -> (view : (m:model) -> vtype (msg m)) -> Application model msg vtype

muvRun : (Application modelType msgType IO) -> IO a
muvRun (MUV model updater view) =
  do
    msg <- view model
    muvRun (MUV (updater model msg) updater view)

-- Sample

data Model = Logouted | Logined String

data MsgOuted = Login String
data MsgIned  = Logout | Greet

total
msgType : Model -> Type
msgType Logouted = MsgOuted
msgType (Logined _) = MsgIned

total
loginPage : IO MsgOuted
loginPage = do
           putStr "Login: "
           map Login getLine

total
genMsg : String -> MsgIned
genMsg "" = Logout
genMsg _ = Greet

total
workPage : String -> IO MsgIned
workPage name = do
           putStr ("Hello, " ++ name ++ "\n")
           putStr "Input empty string for logout or nonempty for greeting\n"
           map genMsg getLine

total
view : (m: Model) -> IO (msgType m)
view Logouted = loginPage
view (Logined name) = workPage name

total
updater : (m:Model) -> (msgType m) -> Model
updater Logouted (Login name) = Logined name
updater (Logined name) Logout = Logouted
updater (Logined name) Greet = Logined name

app : Application Model Main.msgType IO
app = MUV Logouted updater view

main : IO ()
main = muvRun app
