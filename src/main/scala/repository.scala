import scala.util.Random

class repository {

  val rand = Random

  var c = rand.nextInt(20)
  var s = rand.nextInt(20)

  var coke = Product("콜라", c, 1000)
  var sprite = Product("사이다", s, 1500)

  var nameOfCoke = coke.ProductName
  var nameOfSprite = sprite.ProductName

  var numOfCoke = coke.ProductStock
  var numOfSprite = sprite.ProductStock

  var priceOfCoke = coke.ProductPrice
  var priceOfSprite = sprite.ProductPrice

  def printIntro(): Unit = {

    println("\n-------------------------------------------")
    println("****** 자판기 프로그램을 시작합니다! ******\n")
    println("(메뉴) 1.콜라 : " + priceOfCoke + "원, 2.사이다 : " + priceOfSprite + "원\n")
    println("(재고) 1.콜라 : " + numOfCoke + "개,\t" + "2.사이다 : " + numOfSprite + "개\n")
    println("*******************************************")
    println("-------------------------------------------\n")

    println("돈을 넣어주세요.\n")

  }

  def inputMoney(): Int = {
    var inputValue = scala.io.StdIn.readInt()
    inputValue
  }

  def moneyCheck(bb: Int): Int = {
    var isMoney = bb

    def inputCheck(m1: Any) = m1 match {
      case 1 => println(" \n돈을 먼저 넣어주세요.")
      case 2 => println("\n돈을 먼저 넣어주세요.")
      case inputInt: Int => showMenu(bb)
      case _ => println("입력 오류가 발생하였습니다.")
    }

    inputCheck(isMoney)
    isMoney
  }

  def showMenu(m: Int): Unit = {

    var input = m // 처음 받은 돈

    println("-------------------------------------------")
    println("현재 투입된 금액은 총 " + input + "원 입니다.\n")
    println("* 표시가 된 물품은 선택 가능한 물품입니다.")
    println("-------------------------------------------\n")

    // 최소 구매 금액보다 작은 경우 추가 투입

    if (input < coke.ProductPrice) {

      println("(메뉴) 1. " + nameOfCoke + " : ( ), 2. " + nameOfSprite + " : ( )\n")
      println("(재고) 1. " + nameOfCoke + ": " + numOfCoke + "개,   2. " + nameOfSprite + " : " + numOfSprite + "개\n")
      println("-------------------------------------------")
      println("금액이 모자라 선택 할 수 있는 메뉴가 없습니다.\n")
      println("돈을 더 넣어주세요.")
      println("-------------------------------------------")

      var rest = scala.io.StdIn.readInt() //추가로 받는 돈
      showMenu(input + rest) // 현재 투입되어있는 돈에 추가

    }

    else {

      if (input >= coke.ProductPrice && input < sprite.ProductPrice) {

        println("(메뉴) 1. " + nameOfCoke + " : (*), 2. " + nameOfSprite + " : ( )\n")
        println("(재고) 1. " + nameOfCoke + ": " + numOfCoke + "개,   2. " + nameOfSprite + " : " + numOfSprite + "개\n")
        println("-------------------------------------------")
        println("메뉴를 선택해 주세요.")
      }

      else if (input >= sprite.ProductPrice) {

        println("(메뉴) 1. " + nameOfCoke + " : (*), 2. " + nameOfSprite + " : (*)\n")
        println("(재고) 1. " + nameOfCoke + ": " + numOfCoke + "개,   2. " + nameOfSprite + " : " + numOfSprite + "개\n")
        println("-------------------------------------------")
        println("메뉴를 선택해 주세요.")

      }

      println("-------------------------------------------")

      var selection = scala.io.StdIn.readInt()
      purchase(selection, input)
    }

  }


  def purchase(x: Int, y: Int) { //x : 메뉴 번호, y: 받은돈

    var select = x // 선택한 메뉴
    var input = y // 받은 돈
    var output = 0 // 잔돈


    def matchMenu(x: Int): Any = x match {

      case 1 => isEnough(input, coke.ProductName, coke.ProductStock, coke.ProductPrice)

      case 2 => isEnough(input, sprite.ProductName, sprite.ProductStock, sprite.ProductPrice)

      case _ => println("잘못 입력 하셨습니다.")

    }


    def isEnough(input: Int, name: String, num: Int, price: Int): Unit = {

      var input2 = input
      var nameOfSelection = name
      var numOfSelection = num
      var priceOfSelection = price

      if (nameOfSelection == nameOfCoke) numOfSelection = numOfCoke
      else if (nameOfSelection == nameOfSprite) numOfSelection = numOfSprite

      if (input2 < priceOfSelection) {
        println("\n투입한 금액이 부족하여 선택 할 수 없습니다.\n")
        showMenu(input2)
      }

      else {
        println("\n" + nameOfSelection + "를 선택하셨습니다.\n")

        if (numOfSelection <= 0) {
          println("재고가 없어 구매 할 수 없습니다.")
          output = input2
        }

        else {

          output = input2 - priceOfSelection

          if (nameOfSelection == coke.ProductName) numOfCoke = numOfCoke - 1
          else if (nameOfSelection == sprite.ProductName) numOfSprite = numOfSprite - 1

          stockCheck(numOfCoke, numOfSprite)

          println("결제가 정상적으로 처리되었습니다.")
          println("남은 잔액은 " + output + "원 입니다.\n")

        }
      }

    }

    matchMenu(select)

    if (output > 0) {

      println("잔돈을 반환 하시겠습니까?(Y/N)")
      val changeSelect = scala.io.StdIn.readLine()
      change(changeSelect)

    }

    else if (output == 0) {
      println("판매를 종료합니다. 안녕히 가세요.")
    }


    def change(changeSelect: String): Any = changeSelect match {

      case "y" => println("판매를 종료합니다. 잔돈 " + output + "원을 가져가세요.")
        output = 0

      case "n" => showMenu(output)

      case _ => "잘못 입력하셨습니다."

    }

  }


  def stockCheck(a: Int, b: Int): Unit = {

    var stockOfCoke = a
    var stockOfSprite = b

    this.numOfCoke = stockOfCoke
    this.numOfSprite = stockOfSprite

  }

}