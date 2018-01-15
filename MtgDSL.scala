package mtg.dsl

import scala.util.parsing.combinator._
import mtg._

trait LanguageParser{
  val gameM = ""
  val startM = ""
  val withM = ""
  val deckM = ""
  val TM = ""
  val tM = ""
  val turnM = ""
  val firstM = ""
  val secondM = ""
  val thirdM = ""
  val fourthM = ""
  val fifthM = ""
  val playM = ""
}

class EnglishParser extends LanguageParser {
  override val gameM = "game"
  override val startM = "start"
  override val withM = "with"
  override val deckM = "deck"
  override val TM = "T"
  override val tM = "t"
  override val turnM = "turn"
  override val firstM = "first"
  override val secondM = "second"
  override val thirdM = "third"
  override val fourthM = "fourth"
  override val fifthM = "fifth"
  override val playM = "play"
}

object Utils {
  val fileName = "([a-zA-Z0-9\\- ]+)\n".r
  val cardName = "([a-zA-Z0-9\\- ]+)".r
}

class CardNotInDeck(s:String) extends RuntimeException(s)

class MtgParser(lp: LanguageParser) extends JavaTokenParsers{
  var deck:Deck = new Deck
  def mtg = start ~ rep(turn) ^^ {
    case p ~ actions => 
        (p,new ShuffleAction::new DrawSeven::actions.flatten)
  } 
  def start = opt(lp.startM) ~> opt(lp.gameM) ~> opt(lp.withM) ~> opt(lp.deckM) ~> Utils.fileName ^^ {
    case Utils.fileName(a) => 
      this.deck = DeckBuilder.build(s"$a.deck")
      val p = new Player(this.deck)
      p.deck.cards = p.deck.allCards
      p
  }
  def turn =  (turnNum ~> rep(play) <~ ".") ^^ {
    case lst => new DrawAction::lst
  }
  def play = opt(lp.playM) ~> Utils.cardName ^^ {
    case Utils.cardName(s) => 
      if (this.deck.cards.exists(card => card.name == s.toUpperCase))
        new PlayAction(s.toUpperCase)
      else
        throw new CardNotInDeck(s.toUpperCase)
  }
  def turnNum = ((lp.TM | lp.tM | lp.turnM) ~> (wordNumber | wholeNumber))
  def wordNumber = lp.firstM | lp.secondM | lp.thirdM | lp.fourthM | lp.fifthM 
}

object MtgEval {
  def main(args:Array[String]) = {
    args.foreach { filename =>
      val lines = scala.io.Source.fromFile(filename).mkString
      val p = new MtgParser(new EnglishParser)
      p.parseAll(p.mtg,lines) match {
        case p.Success(result,_) => result match {
          case (player,actions) =>
            actions.foreach {a => println(a.toString)}
            println(Simulator.simulate(100,player,actions))
        }
        case p.Failure(s,next) => println(s"Error:  $s at $next")
        case other => println(other)
      }
    }
  }
}

class WrongDeckFormat extends RuntimeException

class DeckParser extends JavaTokenParsers {
  def deck = rep(card) ^^ {
    case lst => val d = new Deck; d.cards = lst; d
  }
  def card = amount ~ Utils.cardName ^^ {
    case a ~ Utils.cardName(s) => new Card(s.toUpperCase, a.toInt) 
  }
  def amount = "[0-9]+".r
}

object DeckBuilder {
  def build(fileName:String):Deck = {
    val p = new DeckParser
    val lines = scala.io.Source.fromFile(fileName).mkString 
    p.parseAll(p.deck,lines) match {
      case p.Success(deck,_) => deck
      case _ => throw new WrongDeckFormat
    }
  }
}

