package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorCapabilities extends App {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case "Hi!" => context.sender() ! "Hello, there!" // replying to a message
      case message: String => println(s"[${context.self.path}] I have received $message")
      case number: Int => println(s"[simple actor] I have received a NUMBER: $number")
      case SpecialMessage(contents) => println(s"[simple actor] I have received something SPECIAL: $contents")
      case SendMessageToYourself(content) =>
        self ! content
      case SayHiTo(ref) => ref ! "Hi!" // alice is being passed as the sender
      case WirelessPhoneMessage(content, ref) => ref forward (content + "s") // i keep the original sender of the WPM
    }
  }

  val system = ActorSystem("actorCapabilitiesDemo")
  val simpleActor = system.actorOf(Props[SimpleActor], "simpleActor")

  simpleActor ! "hello, actor"

  simpleActor ! 42

  case class SpecialMessage(contents: String)

  case class SendMessageToYourself(content: String)

  case class SayHiTo(ref: ActorRef)

  case class WirelessPhoneMessage(content: String, ref: ActorRef)

  simpleActor ! SpecialMessage("some special content")

  simpleActor ! SendMessageToYourself("I am an actor and I am proud of it")

  simpleActor ! SayHiTo(simpleActor)

  val alice = system.actorOf(Props[SimpleActor], "alice")
  val bob = system.actorOf(Props[SimpleActor], "bob")

  alice ! WirelessPhoneMessage("Hi", bob)

}
