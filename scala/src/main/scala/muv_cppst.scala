
package muv_cppst

abstract class View[Message] {
  def run(): Message
}

abstract class Processor {
  def next(): Processor;
}

sealed abstract class Model {
  def processor(): Processor
}


object updaters {
  implicit def logoutedUpdater(model: Logouted, msg: LogoutedMessage): Model = {
    (model, msg) match {
      case (Logouted(), Login(name)) => Logined(name)
    }
  }
  implicit def viewLogouted(model: Logouted) = new View[LogoutedMessage] {
    override def run() : LogoutedMessage = {
     println("Enter name ")
     val name = scala.io.StdIn.readLine()
     Login(name)
    }
  }

  implicit def loginedUpdater(model: Logined, msg: LoginedMessage): Model = {
    (model, msg) match {
      case (Logined(name), Logout()) => Logouted()
      case (Logined(name), Greet()) => model
    }
  }
  implicit def viewLogined(model: Logined) = new View[LoginedMessage] {
    val name = model.name
    override def run() : LoginedMessage = {
     println(s"Hello, $name")
     println("Empty string for logout, nonempy for greeting.")
     scala.io.StdIn.readLine() match {
       case "" => Logout()
       case _ => Greet()
     }
    }
  }
}

import updaters._

class ProcessorImpl[M <: Model, Message](model: M)(implicit updater: (M, Message) => Model, view: M => View[Message]) extends Processor {
  def next(): Processor = {
    val v = view(model)
    val msg = v.run()
    val newModel = updater(model,msg)
    newModel.processor()
  }
}

sealed abstract class LoginedMessage
case class Logout() extends LoginedMessage
case class Greet() extends LoginedMessage

case class Logined(val name: String) extends Model {
  override def processor(): Processor = new ProcessorImpl[Logined, LoginedMessage](this)
}

sealed abstract class LogoutedMessage
case class Login(name: String) extends LogoutedMessage

case class Logouted() extends Model {
  override def processor(): Processor = new ProcessorImpl[Logouted, LogoutedMessage](this)
}

object Main {
  import scala.annotation.tailrec

  @tailrec def process(p: Processor) {
    process(p.next())
  }

  def main(args: Array[String]) = {
    process(new ProcessorImpl[Logouted, LogoutedMessage](Logouted()))
  }
}
