package vendingMachineEx
import scala.util.Random


class Product {


  val rand = Random

  var randomCoke = rand.nextInt(10)
  var randomSprite = rand.nextInt(10)

  case class Menu(productName: String, productStock: Int, productPrice: Int) // 이름, 재고량, 가격

  var coke = Menu("콜라", randomCoke, 1000)
  var sprite = Menu("사이다", randomSprite, 1500)


  var numOfCoke = coke.productStock
  var numOfSprite = sprite.productStock


}
