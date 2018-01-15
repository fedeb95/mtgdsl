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
  override def clone = {
    val p = new Player(this.deck.clone)
    p
  }
}

trait Action{
  def act(p:Player):Boolean
}

class DrawAction extends Action{
  def act(p:Player):Boolean = {
    p.deck.cards match {
        case h::tl => p.hand = h::p.hand; p.deck.cards = tl
        case _ => throw new EmptyDeck
    }
    true
  }
}

class DrawSeven extends Action {
  def act(p:Player):Boolean = {
    0.until(7).forall(_ => (new DrawAction).act(p)) 
  }
}

class PlayAction(val cardName:String) extends Action{
  def act(p:Player):Boolean = {
    p.hand.foldLeft((List[Card](),false))((state,el) => state match{
      case (lst,true) => (el::lst,true) 
      case (lst,false) => if (el.name == cardName) (lst,true) else (el::lst,false)
    }) match{
      case (lst,true) => p.hand = lst;p.battlefield = new Card(cardName,1)::p.battlefield;true
      case (_,false) => false
    }
  }
  override def toString = {
    cardName 
  }
}

class ShuffleAction extends Action{
  def act(p:Player):Boolean = {
    p.deck.cards = Random.shuffle(p.deck.cards)   
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
    actions.foldLeft(true)((state,el)=>state && el.act(p)) 
  }
}

