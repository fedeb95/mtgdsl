package mtg
import scala.util.Random
import scala.math.BigDecimal

case class Card (val name:String, val amount:Int) 
class EmptyDeck extends RuntimeException

class Deck {
  var cards = List[Card]()
  def allCards = {
    var all = List[Card]()
    cards.foldLeft(all)((state,el) => 
        el match { case Card(name,amount) => 
          amount.until(amount*2).foreach { _ =>
            all = new Card(name,1)::all
          };all}) 
  }
  override def clone = {
    val d = new Deck
    d.cards = this.cards
    d
  }
}

class Player(val deck:Deck) {
  var hand = List[Card]()
  var battlefield = List[Card]()
  var looking = List[Card]()
  override def clone = {
    val p = new Player(this.deck.clone)
    p
  }
}

trait DebugAction extends Action{
  abstract override def apply(p:Player):Boolean = {
    val ret = super.apply(p)
    val b = p.battlefield
    val h = p.hand
    println(s"battlefield: $b")
    println(s"hand: $h")
    ret
  }
}

trait Action{
  def apply(p:Player):Boolean
}

class DrawAction extends Action{
  def apply(p:Player):Boolean = {
    p.deck.cards match {
        case h::tl => p.hand = h::p.hand; p.deck.cards = tl
        case _ => throw new EmptyDeck
    }
    true
  }
  override def toString = "DRAW A CARD"
}

class DrawNAction(val n:Int) extends Action{
  def apply(p:Player):Boolean = {
    0.until(n).forall(_ => (new DrawAction).apply(p)) 
  }
  override def toString = s"DRAW $n CARDS"
}

class PlayAction(val cardName:String) extends Action{
  def apply(p:Player):Boolean = {
    p.hand.foldLeft((List[Card](),false))((state,el) => state match{
      case (lst,true) => (el::lst,true) 
      case (lst,false) => if (el.name == cardName) (lst,true) else (el::lst,false)
    }) match{
      case (lst,true) => p.hand = lst;p.battlefield = new Card(cardName,1)::p.battlefield;true
      case (_,false) => false
    }
  }
  override def toString = s"PLAY $cardName"
}

class ShuffleAction extends Action{
  def apply(p:Player):Boolean = {
    p.deck.cards = Random.shuffle(p.deck.cards)   
    true
  }
  override def toString = "SHUFFLE DECK"
}

class ScryAction(n:Int,lst:List[ScrySubAction]) extends Action{
  def apply(p:Player):Boolean = {
    0.until(n).foreach{ _ =>
      p.deck.cards match{
        case h::tl => p.deck.cards = tl;p.looking = h::p.looking 
        case _ => ()
      } 
    } 
    lst.forall(el=>el(p))
    p.deck.cards = p.looking ::: p.deck.cards
    p.looking = List[Card]()
    true
  }
}
abstract class ScrySubAction extends Action
class BottomAction(cardName:String) extends ScrySubAction{
  def apply(p:Player):Boolean = {
    p.looking = p.looking.foldLeft(List[Card]())((state,el)=>
      if (el.name == cardName){
        p.deck.cards = el::((p.deck.cards.reverse).reverse)
        state
      }else 
        el::state)
    true
  }
}

class TopAction(cardName:String) extends ScrySubAction{
  def apply(p:Player):Boolean = {
    p.looking = p.looking.foldLeft(List[Card]())((state,el)=>
      if (el.name == cardName) {
        p.deck.cards ::= el
        state
      }else el::state)
    true
  } 
}

object Simulator {
  def simulate(times:Int,p:Player,actions:List[Action]):Double = {
    (BigDecimal(simulate(times,0,0,p,actions)) / BigDecimal(times))
      .setScale(4,BigDecimal.RoundingMode.HALF_UP).toDouble
  }
  private def simulate(times:Int,count:Int,positive:Int,p:Player,actions:List[Action]):Double = {
    var pos = positive
    val c = count+1
    if (count < times) {
      if (simulateOnce(p.clone,actions)) 
        pos = pos+1
      simulate(times,c,pos,p,actions)
    }else
      pos
  }
  private def simulateOnce(p:Player,actions:List[Action]) = {
    actions.foldLeft(true)((state,el)=>state && el(p)) 
  }
}

