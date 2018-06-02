package mtg.dsl
import scala.util.parsing.combinator._
import mtg._

trait LanguageParser extends JavaTokenParsers{
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
  val drawM = ""
  val cardM = ""
  val aM = ""
  val cardsM = ""
  val topM = ""
  val bottomM = ""
  val scryM = ""
}

trait EnglishParser extends LanguageParser {
  abstract override val TM = "T"
  abstract override val tM = "t"
  abstract override val turnM = "turn"
  abstract override val firstM = "first"
  abstract override val secondM = "second"
  abstract override val thirdM = "third"
  abstract override val fourthM = "fourth"
  abstract override val fifthM = "fifth"
  abstract override val playM = "play"
  abstract override val drawM = "draw"
  abstract override val aM = "a"
  abstract override val cardM = "card"
  abstract override val cardsM = "cards"
  abstract override val topM = "top"
  abstract override val bottomM = "bottom"
  abstract override val scryM = "scry"
}

trait ItalianParser extends LanguageParser {
  abstract override val TM = "T"
  abstract override val tM = "t"
  abstract override val turnM = "turno"
  abstract override val firstM = "primo"
  abstract override val secondM = "secondo"
  abstract override val thirdM = "terzo"
  abstract override val fourthM = "quarto"
  abstract override val fifthM = "quinto"
  abstract override val playM = "gioca"
  abstract override val drawM = "pesca"
  abstract override val aM = "una"
  abstract override val cardM = "carta"
  abstract override val cardsM = "carte"
  abstract override val topM = "sopra"
  abstract override val bottomM = "sotto"
  abstract override val scryM = "profetizza"
}

object Utils {
  val fileName = "([a-zA-Z0-9\\- ]+)\n".r
  val cardName = "([^.\\[\\]\n]+)".r
}

class CardNotInDeck(s:String) extends RuntimeException(s)

class MtgParser(p:Player) extends LanguageParser{
  def mtg = rep(turn) ^^ {
    case actions => 
        (p,new ShuffleAction::new DrawNAction(7)::actions.flatten)
  } 
  def turn =  (turnNum ~> rep(action) <~ ".") ^^ {
    case lst => new DrawAction::lst
  }
  def action = scry | play | draw 
  def scry = scryM ~> wholeNumber ~ ("[" ~> rep(scryActions) <~ "]") ^^ {
    case n ~ lst => new ScryAction(n.toInt,lst) 
  }
  def scryActions:Parser[ScrySubAction] = top | bottom
  def top = topM ~> Utils.cardName ^^ {
    case Utils.cardName(s) => new TopAction(s) 
  }
  def bottom = bottomM ~> Utils.cardName ^^ {
    case Utils.cardName(s) => new BottomAction(s)
  }
  def draw = drawMore | drawOne
  def drawOne = drawM ~ opt(aM) ~ opt(cardM) ^^ {
    case _ => new DrawAction 
  }
  def drawMore = drawM ~> wholeNumber <~ opt(cardsM) ^^ {
    case n => new DrawNAction(n.toInt)
  }
  def play = playM ~> Utils.cardName ^^ {
    case Utils.cardName(s) => 
      if (this.p.deck.cards.exists(card => card.name == s.toUpperCase))
        new PlayAction(s.toUpperCase)
      else
        throw new CardNotInDeck(s.toUpperCase)
  }
  def turnNum = ((TM | tM | turnM) ~> (wordNumber | wholeNumber))
  def wordNumber = firstM | secondM | thirdM | fourthM | fifthM 
}

class WrongArgs extends RuntimeException
class WrongParserInit extends RuntimeException

object MtgEval {
  def main(args:Array[String]) = {
    var player = new Player(new Deck)
    var parser:Option[MtgParser] = None
    val simulations = args.toList match {
      case h1::h2::h3::tl =>
        player = new Player(DeckBuilder.build(s"$h2.deck"))
        player.deck.cards = player.deck.allCards
        if (h1 == "ITA")
          parser = Some(new MtgParser(player) with ItalianParser)
        else
          parser = Some(new MtgParser(player) with EnglishParser)
        h3::tl
      case _ => throw new WrongArgs
    }
    simulations.foreach { filename =>
      val lines = scala.io.Source.fromFile(filename).mkString
      val p = parser match{case Some(parser) => parser; case None => throw new WrongParserInit}
      p.parseAll(p.mtg,lines) match {
        case p.Success(result,_) => result match {
          case (player,actions) =>
            actions.foreach {a => println(a.toString)}
            println(Simulator.simulate(1000,player,actions))
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

