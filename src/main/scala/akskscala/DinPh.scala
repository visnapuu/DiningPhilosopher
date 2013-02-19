package net.jakubkorab.philosophers

import messages._
import scala.actors._
import scala.actors.Actor._
import scala.math._

package messages {

import collection.script.Message

class Chopstick(val position : Int)

object Side extends Enumeration {
  type Side = Value
  val Left, Right = Value

  def randomSide() = { Side(floor(Side.values.size * random).intValue) }
  def otherSide(side : Side.Value) = { Side.values.find{_ != side}.get }
}

sealed abstract class Message

abstract class TableMessage() extends Message
case class AllFinished() extends TableMessage

abstract class ChopstickResponse() extends TableMessage
case class ChopstickAvailable(val chopstick : Chopstick) extends ChopstickResponse
case class ChopstickUnavailable() extends ChopstickResponse

abstract class DinerMessage() extends Message
case class RequestChopstick(val philosopher : Philosopher, val side : Side.Value) extends DinerMessage
case class ReplaceChopstick(val chopstick : Chopstick) extends DinerMessage
case class CouldNotEatAnotherBite(val guest : String) extends DinerMessage
}

class Philosopher(val name : String, val wordsOfWisdom : String) extends Actor {
  var table : Actor = null
  var seatedAt : Int = -1

  private var timesLeftToEat = 3
  override def act() = {
    while (timesLeftToEat > 0) {
      think()
      val side = Side.randomSide // pick a chopstick to use first
      say("Requesting chopstick 1 on " + side)
      table !? (1000, RequestChopstick(this, side)) match {
        case Some(ChopstickAvailable(chopstick1 : Chopstick)) => {
          pause() // put in a delay so we can see actors switching
          val otherSide = Side.otherSide(side) // request the other
          say("Requesting chopstick 2 on " + otherSide)
          table !? (100, RequestChopstick(this, otherSide)) match {
            case Some(ChopstickAvailable(chopstick2 : Chopstick)) => {
              eat()
              pause()
              table ! ReplaceChopstick(chopstick1) // return chopsticks
              pause()
              table ! ReplaceChopstick(chopstick2)
            }
            case Some(ChopstickUnavailable()) => {
              say("No " + otherSide + " chopstick");
              table ! ReplaceChopstick(chopstick1);
            }
            case None => { say("None"); table ! ReplaceChopstick(chopstick1) }
          }
        }
        case Some(ChopstickUnavailable()) => { say("No " + side + " chopstick") } // no luck getting a chopstick
        case None => say("None")
      }
    }
    react {
      case AllFinished => { say(wordsOfWisdom); exit }
    }
  }

  private def think() = { say("Hmm"); pause() }
  private def eat() = {
    say("Nom nom");
    timesLeftToEat -= 1
    if (timesLeftToEat == 0) {
      table ! CouldNotEatAnotherBite(name)
    }
  }
  private def say(s : String) = { println(name + ": " + s) }
  private def pause() = { Thread.sleep(ceil(random * 1000).intValue) }
}
object Philosopher {
  def apply(name : String, wordsOfWisdom : String) = new Philosopher(name, wordsOfWisdom)
}

class Table(val philosophers : Set[Philosopher]) extends Actor {
  if (philosophers.size < 2) throw new IllegalArgumentException("At least 2 philosophers must dine together")
  var chopsticks = new Array[Chopstick](philosophers.size)
  var location = 0
  philosophers.foreach { philosopher =>
    chopsticks(location) = new Chopstick(location)  // lay the cutlery
    philosopher.seatedAt = location
    location += 1
  }
  var guestsEating = philosophers.size

  override def act() = {
    println("Starting the meal")
    philosophers.foreach { philosopher => philosopher.table = self; philosopher.start  } // let's go
    while (true) {
      receive {
        case RequestChopstick(philosopher : Philosopher, side : Side.Value) => giveChopstickIfAvailable(philosopher, side)
        case ReplaceChopstick(chopstick : Chopstick) => replaceChopstick(chopstick)
        case CouldNotEatAnotherBite(guest : String) => guestFinished(guest)
      }
    }
  }

  private def giveChopstickIfAvailable(philosopher : Philosopher, side : Side.Value) = {
    var index = if (side == Side.Right) philosopher.seatedAt else philosopher.seatedAt - 1
    if (index < 0) { index = philosophers.size - 1 } // get the one on the end of the array

    val chopstick = chopsticks(index)
    if (chopstick == null) {
      println("No chopstick available at " + index)
      sender ! ChopstickUnavailable() // sender, not philosopher!
    } else {
      chopsticks(index) = null
      sender ! ChopstickAvailable(chopstick)
    }
  }

  private def replaceChopstick(chopstick : Chopstick) = {
    chopsticks(chopstick.position) = chopstick
  }

  private def guestFinished(guest : String ) = {
    println(guest + " is done")
    guestsEating -= 1
    if (guestsEating == 0) {
      philosophers.foreach {_ ! AllFinished}
      println("All done")
      exit
    }
  }
}

object PhilosophersLauncher {
  def main(args : Array[String]) = {
    val table = new Table(
      Set(Philosopher("Seneca the Younger", "The point is, not how long you live, but how nobly you live."),
        Philosopher("Epictetus", "Freedom is secured not by the fulfilling of men's desires, but by the removal of desire." ),
        Philosopher("Marcus Aurelius", "Everything is right for me, which is right for you, O Universe."),
        Philosopher("Zeno of Citium", "Shit happens.")) // one of his lesser known ones
    ).start
  }
}