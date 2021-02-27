package vendingMachineEx

class VendingMachine(product: Product) {

  var additionalMoney = 0
  var output = 0 // 잔돈 초기화

  var isBuyableCoke = " "
  var isBuyableSprite = " "

  var selectionOfMenu: Boolean = true
  var isLackOfMoney: Boolean = false
  var selectToReturnChange = "y"

  def receivedValueIsMoney(receivedMoney: Int): Boolean = receivedMoney match {
    case 1 => {
      println(" \n돈을 먼저 넣어주세요.");
      false
    }

    case 2 => {
      println("\n돈을 먼저 넣어주세요.");
      false
    }

    case isMoney: Int => {
      if (isMoney < 0) {
        println("돈을 넣어주세요.");
        false
      }
      else true
    }

    case _ => println("입력 오류가 발생하였습니다."); false

  }


  def checkSelectableProducts(receivedMoney: Int): Unit = {
    if((receivedMoney >= product.coke.productPrice) && product.numOfCoke > 0) {
      isBuyableCoke = "*"
    }

    if((receivedMoney >= product.sprite.productPrice) && product.numOfSprite > 0) {
      isBuyableSprite = "*"
    }
  }

  def showBuyableProducts(input: Int): Unit = {
    println("-------------------------------------------")
    println("(메뉴) 1. " + product.coke.productName + " : (" + isBuyableCoke + "), 2. " + product.sprite.productName + " : (" + isBuyableSprite + ")\n")
    println("(재고) 1. " + product.coke.productName + ": " + product.numOfCoke + "개,   2. " + product.sprite.productName + " : " + product.numOfSprite + "개")
    println("-------------------------------------------")

  }

  def selectToBuyProduct(receivedMoney: Int, selectionOfProduct: Int): Unit = {
    selectionOfProduct match {

      case 1 => {
        isMoneyEnoughToBuy(receivedMoney, product.coke.productName, product.numOfCoke, product.coke.productPrice)
        selectionOfMenu = false
      }

      case 2 => {
        isMoneyEnoughToBuy(receivedMoney, product.sprite.productName, product.numOfSprite, product.sprite.productPrice)
        selectionOfMenu = false
      }

      case _ => {
        println("잘못 입력 하셨습니다. 다시 선택해 주세요.")
        selectionOfMenu = true

      }
    }

  }

  def isMoneyEnoughToBuy(receivedMoney: Int, nameOfSelection: String, numOfSelection: Int, priceOfSelection: Int): Unit = {
    if (receivedMoney < priceOfSelection) {
      println("\n-------------------------------------------")
      println("투입한 금액이 부족하여 선택 할 수 없습니다.\n")
      isLackOfMoney = true
    }

    else {
      println("-------------------------------------------")
      println(nameOfSelection + "를 선택하셨습니다.\n")

      if (numOfSelection <= 0) {
        println("재고가 없어 구매 할 수 없습니다.")
        showBuyableProducts(receivedMoney)
      }

      else {
        reduceStocksOfProducts(nameOfSelection)
        println("결제가 정상적으로 처리되었습니다.")
        println("-------------------------------------------")
        this.output = receivedMoney - priceOfSelection
      }
    }
  }

  def askAdditionalMoney(receivedMoney: Int, selectToAddMoney: String): Any = selectToAddMoney match {
    case "y" => {
      printf("\n돈을 추가해주세요.\n")

      val addedMoney = scala.io.StdIn.readInt
      this.additionalMoney = addedMoney
      selectionOfMenu = true

      checkSelectableProducts(receivedMoney)
    }

    case "n" => askContinueBuying(receivedMoney)

    case _ => {
      println("잘못 입력하셨습니다.")
      isLackOfMoney = true
    }
  }


  def askContinueBuying(output: Int): Unit = {
    if (output > 0) {
      println("남은 잔액은 " + output + "원 입니다.\n")
      println("잔돈을 반환 하시겠습니까?(Y/N)")
      println("-------------------------------------------")

      val selectToGetChange = scala.io.StdIn.readLine()
      selectToReturnChange = selectToGetChange
    }

    else if (output == 0) println("판매를 종료합니다.\n안녕히 가세요.")

  }


  def hereIsYourChange(selecToGetChange: String): Any = selecToGetChange match {
    case "y" => {
      println("\n판매를 종료합니다. 잔돈 " + output + "원을 가져가세요.\n")
      println("안녕히 가세요.")
      selectionOfMenu = false
      output = 0
    }

    case "n" => {
      selectionOfMenu = true
      output
    }

    case _ => {
      println("잘못 입력하셨습니다.")
      askContinueBuying(output)
    }
  }


  def reduceStocksOfProducts(nameOfSelection: String): Unit = {
    if (nameOfSelection == product.coke.productName) this.product.numOfCoke = this.product.numOfCoke - 1
    else if (nameOfSelection == product.sprite.productName) this.product.numOfSprite = this.product.numOfSprite - 1

  }
}
