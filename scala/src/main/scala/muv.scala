
package muv

sealed abstract class MsgLogouted
case class Login(name: String) extends MsgLogouted

sealed abstract class MsgLogined
case class Logout() extends MsgLogined
case class Greet() extends MsgLogined

sealed abstract class Model {
  type Message
  def view() : View[Message]
}

case class Logouted() extends Model {
  type Message = MsgLogouted
  override def view() : View[Message] = new View[Message] {
   override def run() = {
     println("Enter name ")
     val name = scala.io.StdIn.readLine()
     Login(name)
   }
  }
}

case class Logined(name: String) extends Model {
  type Message = MsgLogined
  override def view() : View[Message] = new View[Message] {
   override def run() = {
     println(s"Hello, $name")
     println("Empty string for logout, nonempy for greeting.")
     scala.io.StdIn.readLine() match {
       case "" => Logout()
       case _ => Greet()
     }
   }
  }
}

abstract class View[Msg] {
  def run() : Msg
}

object Viewer {
  def view(model: Model): View[model.Message] = {
   model.view() 
//    model match {
//      case Logouted() => new View[MsgLogouted] { override def run() = Login("qq") }
//      case Logined(name) => new View { override def run() = Logout() }
//    }
  }
}

object Updater {
  def update(model: Model)(msg: model.Message) : Model = {
    model match {
      case Logouted() => msg match {
        case Login(name) => Logined(name)
      }
      case Logined(name) => msg match {
        case Logout() => Logouted()
        case Greet() => model
      }
    }
  }
}

object Main {
  import scala.annotation.tailrec

  @tailrec def process(m: Model) {
    val msg = Viewer.view(m).run()
    process(Updater.update(m)(msg))
  }

  def main(args: Array[String]) = {
    process(Logouted())
  }
}
