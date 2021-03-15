package checkout

import checkout.Checkout.{MultiPrice, Price}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckoutTotalPriceSpec extends AnyFlatSpec with Matchers {

  val pricing = Map(
    "A" -> Price(50, Some(MultiPrice(3, 130))),
    "B" -> Price(30, Some(MultiPrice(2, 45))),
    "C" -> Price(20, None),
    "D" -> Price(15, None)
  )

  val checkout = new Checkout(pricing)

  "Checkout Total" should "be 0 when no items scanned" in {
    checkout.total shouldBe 0
  }

  it should "be price of item when one item scanned" in {
    checkout.scan("B")
    checkout.total shouldBe 30
  }

  it should "add the price of another item" in {
    checkout.scan("C")
    checkout.total shouldBe 50
  }

  it should "add the price when the same item is added" in {
    checkout.scan("C")
    checkout.total shouldBe 70
  }

  it should "take into account multiprice when n items are brought for special price" in {
    checkout.scan("B")
    checkout.total shouldBe 85
  }

  it should "take into account multiprice when a multiple of n items are brought for special price" in {
    checkout.scan("B")
    checkout.scan("B")
    checkout.total shouldBe 130
  }

  it should "discard any items which do not have a price" in {
    checkout.scan("N")
    checkout.total shouldBe 130
  }

  def checkIfCorrectTotal(items: List[String], total: Int) =
    it should s"calculate the total for items $items to be $total" in {
    val tillCheckout = new Checkout(pricing)
    items.foreach(tillCheckout.scan)
    tillCheckout.total shouldBe total
  }

  checkIfCorrectTotal(List.empty, 0)
  checkIfCorrectTotal(List("A"), 50)
  checkIfCorrectTotal(List("A", "A", "B"), 130)
  checkIfCorrectTotal(List("A", "C", "B"), 100)
  checkIfCorrectTotal(List("A", "B", "B"), 95)
  checkIfCorrectTotal(List("A", "A", "B", "C", "C", "A", "D"), 215)
  checkIfCorrectTotal(List("A", "A", "B", "A", "A"), 210)
  checkIfCorrectTotal(List("C", "D", "B", "A"), 115)
  checkIfCorrectTotal(List("B", "D", "D", "B", "A", "B"), 155)
  checkIfCorrectTotal(List("D", "A", "B", "A", "B", "A", "A", "C", "A", "A"), 340)
  checkIfCorrectTotal(List("A", "A", "B", "B"), 145)


}
