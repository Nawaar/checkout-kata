package checkout

import checkout.Checkout.{MultiPrice, Price}

import scala.collection.mutable
import scala.math.Integral.Implicits._

class Checkout(pricing: Map[String, Price]) {

  private val checkoutItems: mutable.Map[String, Int] = mutable.Map()

  def total: Int = checkoutItems.foldLeft(0)((total, item) => total + totalItemPrice(item._1, item._2))

  private def totalItemPrice(item: String, quantity: Int): Int = pricing.get(item) match {
    case Some(Price(unitPrice, None)) => unitPrice * quantity
    case Some(Price(unitPrice, Some(MultiPrice(amount, multiPrice)))) =>
      val (batch, individual) = quantity /% amount
      (batch * multiPrice) + (individual * unitPrice)
    case None => 0
  }

  def scan(sku: String): Unit = if (pricing.contains(sku)) checkoutItems.get(sku) match {
    case Some(amount) => checkoutItems.update(sku, amount + 1)
    case None => checkoutItems.update(sku, 1)
  }

}

object Checkout {
  
  case class Price(unit: Int, multiPrice: Option[MultiPrice])

  case class MultiPrice(amount: Int, price: Int)
  
}