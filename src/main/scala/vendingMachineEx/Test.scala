package vendingMachineEx

class Test extends Product{


  def inputMoney(): Int = {

    println("\n-------------------------------------------")
    println("****** 자판기 프로그램을 시작합니다! ******\n")
    println("(메뉴) 1.콜라 : " + coke.productPrice + "원, 2.사이다 : " + sprite.productPrice + "원\n")
    println("(재고) 1.콜라 : " + numOfCoke + "개,\t" + "2.사이다 : " + numOfSprite + "개\n")
    println("*******************************************")
    println("-------------------------------------------\n")

    println("돈을 넣어주세요.\n")

    val inputValue = scala.io.StdIn.readInt()
    inputValue

  }

  def inputCheck(isMoney: Int) = isMoney match {

    case 1 => println(" \n돈을 먼저 넣어주세요.")

    case 2 => println("\n돈을 먼저 넣어주세요.")

    case input: Int => {
      if (input < 0) println("돈을 넣어주세요.")
      else showMenu(isMoney)
    }

    case _ => println("입력 오류가 발생하였습니다.")

  }


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
      println("금액이 모자라 선택 할 수 있는 메뉴가 없습니다.\n")
      println("돈을 더 넣어주세요.")
      println("-------------------------------------------")

      val rest = scala.io.StdIn.readInt() //추가로 받는 돈
      showMenu(input + rest) // 현재 투입되어있는 돈에 추가

    }


    // 최소 구매 가능 금액 이상 돈을 투입한 경우, 구매가능 상품 출력

    else {

      if (input >= coke.productPrice && input < sprite.productPrice) {

        println("(메뉴) 1. " + coke.productName + " : (*), 2. " + sprite.productName + " : ( )\n")
        println("(재고) 1. " + coke.productName + ": " + numOfCoke + "개,   2. " + sprite.productName + " : " + numOfSprite + "개\n")
        println("-------------------------------------------")
        println("메뉴를 선택해 주세요.")
      }

      else if (input >= sprite.productPrice) {

        println("(메뉴) 1. " + coke.productName + " : (*), 2. " + sprite.productName + " : (*)\n")
        println("(재고) 1. " + coke.productName + ": " + numOfCoke + "개,   2. " + sprite.productName + " : " + numOfSprite + "개\n")
        println("-------------------------------------------")
        println("메뉴를 선택해 주세요.")

      }

      println("-------------------------------------------")

      var selection = scala.io.StdIn.readLine()
      purchase(selection, input)
    }

  }


  def purchase(select: Any, input: Int) { //select : 선택한 메뉴 번호, input: 받은돈

    var output = 0 // 잔돈

    def matchMenu(x: Any): Any = x match {

      case 1 => isEnough(input, coke.productName, numOfCoke, coke.productPrice)

      case 2 => isEnough(input, sprite.productName, numOfSprite , sprite.productPrice)

      case _ => { println("잘못 입력 하셨습니다. 다시 선택해 주세요.")
                   showMenu(input)
                 }

    }


    // 최소 구매 가능 금액과 비교
    // 재고 상태 확인

    def isEnough(input: Int, name: String, num: Int, price: Int): Unit = {


      var nameOfSelection = name
      var numOfSelection = num
      var priceOfSelection = price

      if (nameOfSelection == coke.productName) numOfSelection = numOfCoke
      else if (nameOfSelection == sprite.productName) numOfSelection = numOfSprite

      if (input < priceOfSelection) {
        println("\n투입한 금액이 부족하여 선택 할 수 없습니다.\n")
        addMoney(input)
        //showMenu(input2)
      }

      else {
        println("\n" + nameOfSelection + "를 선택하셨습니다.\n")

        if (numOfSelection <= 0) {
          println("재고가 없어 구매 할 수 없습니다.")
          showMenu(input)
        }

        else {

          output = input - priceOfSelection

          if (nameOfSelection == coke.productName) this.numOfCoke = this.numOfCoke - 1
          else if (nameOfSelection == sprite.productName) this.numOfSprite = this.numOfSprite - 1

          stockCheck(numOfCoke, numOfSprite )

          println("결제가 정상적으로 처리되었습니다.")
          changeAsk(output)

        }
      }

    }

    matchMenu(select)


    // 금액 추가 확인

    def addMoney(input: Int): Unit = {

      var passed = input

      println("돈을 추가 하시겠습니까? (Y/N)")
      println("-------------------------------------------\n")

      val addSelect = scala.io.StdIn.readLine
      addAsk(addSelect)

      def addAsk(addSelect: String): Any = addSelect match {

        case "y" => {
          printf("돈을 추가해주세요.\n")

          var addMoney = scala.io.StdIn.readInt

          passed = passed + addMoney // error :: addMoney : reassignment to val

          showMenu(passed)
        }

        case "n" => changeAsk(passed)

        case _ => println("잘못 입력하셨습니다.")
      }

    }

    // 잔돈 반환 처리

    def changeAsk(output: Int): Unit = {

      var changeMoney = output

      if (changeMoney > 0) {

        println("남은 잔액은 " + output + "원 입니다.\n")
        println("잔돈을 반환 하시겠습니까?(Y/N)")
        println("-------------------------------------------\n")

        var changeSelect = scala.io.StdIn.readLine()
        change(changeSelect)

      }

      else if (changeMoney == 0) {
        println("판매를 종료합니다. 안녕히 가세요.")
      }


      def change(changeSelect: String): Any = changeSelect match {

        case "y" => {
          println("\n판매를 종료합니다. 잔돈 " + changeMoney + "원을 가져가세요.")
          println("안녕히 가세요.")
          changeMoney = 0
        }

        case "n" => showMenu(changeMoney)

        case _ => "잘못 입력하셨습니다."

      }
    }

  }

  def stockCheck(a: Int, b: Int): Unit = {

    this.numOfCoke = a
    this.numOfSprite = b

  }
}
