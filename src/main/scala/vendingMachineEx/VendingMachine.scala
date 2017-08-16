package vendingMachineEx

class VendingMachine(product: Product) {

  val output = 0 // 잔돈 초기화


  def inputMoney(): Int = {

    println("\n-------------------------------------------")
    println("****** 자판기 프로그램을 시작합니다! ******\n")
    println("(메뉴) 1.콜라 : " + product.coke.productPrice + "원, 2.사이다 : " + product.sprite.productPrice + "원\n")
    println("(재고) 1.콜라 : " + product.numOfCoke + "개,\t" + "2.사이다 : " + product.numOfSprite + "개\n")
    println("*******************************************")
    println("-------------------------------------------\n")

    println("돈을 넣어주세요.")

    val inputValue = scala.io.StdIn.readInt()
    inputValue

  }


  def inputCheck(isMoney: Int) = isMoney match {

    case 1 => println(" \n돈을 먼저 넣어주세요.")

    case 2 => println("\n돈을 먼저 넣어주세요.")

    case isMoney: Int => {
      if (isMoney < 0) println("돈을 넣어주세요.")
      else showMenu(isMoney)
    }

    case _ => println("입력 오류가 발생하였습니다.")

  }


  def showMenu(input: Int): Unit = {

    println("-------------------------------------------")
    println("현재 투입된 금액은 총 " + input + "원 입니다.\n")
    println("* 표시가 된 물품은 선택 가능한 물품입니다.")
    println("-------------------------------------------\n")


    if (input < product.coke.productPrice) {

      println("(메뉴) 1. " + product.coke.productName + " : ( ), 2. " + product.sprite.productName + " : ( )\n")
      println("(재고) 1. " + product.coke.productName + ": " + product.numOfCoke + "개,   2. " + product.sprite.productName + " : " + product.numOfSprite + "개\n")
      println("-------------------------------------------")
      println("금액이 모자라 선택 할 수 있는 메뉴가 없습니다.\n돈을 더 넣어주세요.")
      println("-------------------------------------------")

      val rest = scala.io.StdIn.readInt()
      showMenu(input + rest)

    }


    else {

      if ((input >= product.coke.productPrice) && (input < product.sprite.productPrice)) {

        if (product.numOfCoke != 0) println("(메뉴) 1. " + product.coke.productName + " : (*), 2. " + product.sprite.productName + " : ( )\n")

        else {
          println("(메뉴) 1. " + product.coke.productName + " : ( ), 2. " + product.sprite.productName + " : ( )\n")
        }

        println("(재고) 1. " + product.coke.productName + ": " + product.numOfCoke + "개,   2. " + product.sprite.productName + " : " + product.numOfSprite + "개\n")
        println("-------------------------------------------")
        println("메뉴를 선택해 주세요.")
      }

      else if (input >= product.sprite.productPrice) {

        if ((product.numOfCoke == 0) && (product.numOfSprite != 0)) println("(메뉴) 1. " + product.coke.productName + " : ( ), 2. " + product.sprite.productName + " : (*)\n")

        else if ((product.numOfCoke != 0) && (product.numOfSprite == 0)) println("(메뉴) 1. " + product.coke.productName + " : (*), 2. " + product.sprite.productName + " : ( )\n")
        else if ((product.numOfCoke == 0) && (product.numOfSprite == 0)) {
          println("(메뉴) 1. " + product.coke.productName + " : ( ), 2. " + product.sprite.productName + " : ( )\n")
          println("-------------------------------------------")
          println("모든 제품의 재고가 없어 판매를 종료합니다.\n")
          println("잔돈 " + input + "을 가져가세요.")
          println("-------------------------------------------")
          System.exit(1)
        }

        else {
          println("(메뉴) 1. " + product.coke.productName + " : (*), 2. " + product.sprite.productName + " : (*)\n")
        }

        println("(재고) 1. " + product.coke.productName + ": " + product.numOfCoke + "개,   2. " + product.sprite.productName + " : " + product.numOfSprite + "개\n")
        println("-------------------------------------------")
        println("메뉴를 선택해 주세요.")

      }

      val selection = scala.io.StdIn.readInt()
      purchase(selection, input)
    }

  }


  def purchase(selection: Int, input: Int) {

    selection match {

      case 1 => isEnough(input, product.coke.productName, product.numOfCoke, product.coke.productPrice)

      case 2 => isEnough(input, product.sprite.productName, product.numOfSprite, product.sprite.productPrice)

      case _ => {
        println("잘못 입력 하셨습니다. 다시 선택해 주세요.")
        showMenu(input)
      }
    }

  }


  /*
    def purchase(selection: Int, input: Int) {

      var passed = input

      def matchMenu(passed: Int): Any = passed match {

        case 1 => isEnough(input, product.coke.productName, product.numOfCoke, product.coke.productPrice)

        case 2 => isEnough(input, product.sprite.productName, product.numOfSprite, product.sprite.productPrice)

        case _ => {
          println("잘못 입력 하셨습니다. 다시 선택해 주세요.")
          showMenu(input)
        }
      }

      matchMenu(selection)
    }
  */


  def isEnough(input: Int, nameOfSelection: String, numOfSelection: Int, priceOfSelection: Int): Unit = {

    if (input < priceOfSelection) {
      println("\n-------------------------------------------")
      println("투입한 금액이 부족하여 선택 할 수 없습니다.\n")
      addMoney(input)
    }

    else {
      println("-------------------------------------------")
      println("\n" + nameOfSelection + "를 선택하셨습니다.\n")

      if (numOfSelection <= 0) {
        println("재고가 없어 구매 할 수 없습니다.")
        showMenu(input)
      }

      else {
        //output = input - priceOfSelection
        stockCheck(nameOfSelection)
        println("결제가 정상적으로 처리되었습니다.")
        changeAsk(input - priceOfSelection)
      }

    }
  }


  def addMoney(input: Int): Unit = {

    println("돈을 추가 하시겠습니까? (Y/N)")
    println("-------------------------------------------")

    val addSelect = scala.io.StdIn.readLine
    addAsk(addSelect)

    def addAsk(addSelect: String): Any = addSelect match {

      case "y" => {
        printf("돈을 추가해주세요.\n")
        val addMoney = scala.io.StdIn.readInt
        showMenu(input + addMoney)
      }

      case "n" => changeAsk(input)

      case _ => {
        println("잘못 입력하셨습니다.")
        addMoney(input)
      }
    }

  }


  def changeAsk(output: Int): Unit = {

    if (output > 0) {
      println("\n-------------------------------------------")
      println("남은 잔액은 " + output + "원 입니다.\n")
      println("잔돈을 반환 하시겠습니까?(Y/N)")
      println("-------------------------------------------")

      val changeSelect = scala.io.StdIn.readLine()
      //change(changeSelect)
      changeSelect
    }

    else if (output == 0) println("판매를 종료합니다. 안녕히 가세요.")

  }




  def change(changeSelect: String): Any = changeSelect match {

    case "y" => {
      println("\n-------------------------------------------")
      println("\n판매를 종료합니다. 잔돈 " + output + "원을 가져가세요.\n")
      println("안녕히 가세요.")
    }

    case "n" => {
      showMenu(output)
    }

    case _ => {
      println("잘못 입력하셨습니다.")
      changeAsk(output)
    }
  }





  def stockCheck(nameOfSelection: String): Unit = {

    if (nameOfSelection == product.coke.productName) this.product.numOfCoke = this.product.numOfCoke - 1
    else if (nameOfSelection == product.sprite.productName) this.product.numOfSprite = this.product.numOfSprite - 1

  }
}
