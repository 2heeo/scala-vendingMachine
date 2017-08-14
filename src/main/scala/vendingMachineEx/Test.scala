package vendingMachineEx

class Test extends Product {

  val output = 0 // 잔돈 초기화

  // 처음 시작화면 출력, 돈 투입 시도
  def inputMoney(): Int = {

    println("\n-------------------------------------------")
    println("****** 자판기 프로그램을 시작합니다! ******\n")
    println("(메뉴) 1.콜라 : " + coke.productPrice + "원, 2.사이다 : " + sprite.productPrice + "원\n")
    println("(재고) 1.콜라 : " + numOfCoke + "개,\t" + "2.사이다 : " + numOfSprite + "개\n")
    println("*******************************************")
    println("-------------------------------------------\n")

    println("돈을 넣어주세요.")

    val inputValue = scala.io.StdIn.readInt()
    inputValue

  }


  // 투입된 값이 돈인지 메뉴인지 체크
  def inputCheck(isMoney: Int) = isMoney match {

    case 1 => println(" \n돈을 먼저 넣어주세요.")

    case 2 => println("\n돈을 먼저 넣어주세요.")

    case isMoney: Int => {
      if (isMoney < 0) println("돈을 넣어주세요.")
      else showMenu(isMoney)
    }

    case _ => println("입력 오류가 발생하였습니다.")

  }


  // 현재 투입된 돈으로 구매 할 수 있는 상품 출력
  def showMenu(input: Int): Unit = {

    println("-------------------------------------------")
    println("현재 투입된 금액은 총 " + input + "원 입니다.\n")
    println("* 표시가 된 물품은 선택 가능한 물품입니다.")
    println("-------------------------------------------\n")


    // 최소 구매 가능 금액보다 작은 경우 추가 투입 요구
    if (input < coke.productPrice) {

      println("(메뉴) 1. " + coke.productName + " : ( ), 2. " + sprite.productName + " : ( )\n")
      println("(재고) 1. " + coke.productName + ": " + numOfCoke + "개,   2. " + sprite.productName + " : " + numOfSprite + "개\n")
      println("-------------------------------------------")
      println("금액이 모자라 선택 할 수 있는 메뉴가 없습니다.\n 돈을 더 넣어주세요.")
      println("-------------------------------------------")

      val rest = scala.io.StdIn.readInt()
      showMenu(input + rest)

    }


    // 최소 구매 가능 금액 이상 돈을 투입한 경우, 재고 상태 고려한 구매가능 상품 출력
    else {

      if ((input >= coke.productPrice) && (input < sprite.productPrice)) {

        if (numOfCoke != 0) println("(메뉴) 1. " + coke.productName + " : (*), 2. " + sprite.productName + " : ( )\n")

        else {
          println("(메뉴) 1. " + coke.productName + " : ( ), 2. " + sprite.productName + " : ( )\n")
        }

        println("(재고) 1. " + coke.productName + ": " + numOfCoke + "개,   2. " + sprite.productName + " : " + numOfSprite + "개\n")
        println("-------------------------------------------")
        println("메뉴를 선택해 주세요.")
      }

      else if (input >= sprite.productPrice) {

        if ((numOfCoke == 0) && (numOfSprite != 0)) println("(메뉴) 1. " + coke.productName + " : ( ), 2. " + sprite.productName + " : (*)\n")

        else if ((numOfCoke != 0) && (numOfSprite == 0)) println("(메뉴) 1. " + coke.productName + " : (*), 2. " + sprite.productName + " : ( )\n")
        else if ((numOfCoke == 0) && (numOfSprite == 0)) {
          println("(메뉴) 1. " + coke.productName + " : ( ), 2. " + sprite.productName + " : ( )\n")
          println("-------------------------------------------")
          println("모든 제품의 재고가 없어 판매를 종료합니다.\n")
          println("잔돈 " + input + "을 가져가세요.")
          println("-------------------------------------------")
          System.exit(1)
        }

        else {
          println("(메뉴) 1. " + coke.productName + " : (*), 2. " + sprite.productName + " : (*)\n")
        }

        println("(재고) 1. " + coke.productName + ": " + numOfCoke + "개,   2. " + sprite.productName + " : " + numOfSprite + "개\n")
        println("-------------------------------------------")
        println("메뉴를 선택해 주세요.")

      }

      val selection = scala.io.StdIn.readInt()
      purchase(selection, input)
    }

  }


  // 상품 구매 시도
  def purchase(selection: Int, input: Int) {

    var passed = input
    var output = 0 // 잔돈

    def matchMenu(passed: Int): Any = passed match {

      case 1 => isEnough(input, coke.productName, numOfCoke, coke.productPrice)

      case 2 => isEnough(input, sprite.productName, numOfSprite, sprite.productPrice)

      case _ => {
        println("잘못 입력 하셨습니다. 다시 선택해 주세요.")
        showMenu(input)
      }
    }

    matchMenu(selection)
  }


  // 최소 구매 가능 금액과 비교
  // 재고 상태 확인
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


  // 금액 추가 여부 확인
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


  // 잔돈 반환 처리
  def changeAsk(output: Int): Unit = {

    if (output > 0) {
      println("\n-------------------------------------------")
      println("남은 잔액은 " + output + "원 입니다.\n")
      println("잔돈을 반환 하시겠습니까?(Y/N)")
      println("-------------------------------------------")

      val changeSelect = scala.io.StdIn.readLine()
      change(changeSelect)
    }

    else if (output == 0) println("판매를 종료합니다. 안녕히 가세요.")


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

  }


  // 재고 처리
  def stockCheck(nameOfSelection: String): Unit = {

    if (nameOfSelection == coke.productName) this.numOfCoke = this.numOfCoke - 1
    else if (nameOfSelection == sprite.productName) this.numOfSprite = this.numOfSprite - 1

  }
}
