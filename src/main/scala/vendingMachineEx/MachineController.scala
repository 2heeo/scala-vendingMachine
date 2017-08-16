package vendingMachineEx


class MachineController {

  def control: Unit = {

    val product = new Product

    val vendingMachine = new VendingMachine(product)

    while (true) {

      println("\n-------------------------------------------")
      println("****** 자판기 프로그램을 시작합니다! ******\n")
      println("(메뉴) 1.콜라 : " + product.coke.productPrice + "원, 2.사이다 : " + product.sprite.productPrice + "원\n")
      println("(재고) 1.콜라 : " + product.numOfCoke + "개,\t" + "2.사이다 : " + product.numOfSprite + "개\n")
      println("*******************************************")
      println("-------------------------------------------\n")


      println("돈을 넣어주세요.")
      val receivedValue = scala.io.StdIn.readInt()

      val isMoney = vendingMachine.receivedValueIsMoney(receivedValue)

      if (isMoney) { // isMoney == true

//        println("received Value : " + receivedValue)
        var receivedMoney = receivedValue
        vendingMachine.output = receivedMoney
//        println("received Money: " + receivedMoney)


        while (vendingMachine.selectionOfMenu) {

          println("-------------------------------------------")
          println("현재 투입된 금액은 총 " + receivedMoney + "원 입니다.\n")
          println("* 표시가 된 물품은 선택 가능한 물품입니다.\n")

          vendingMachine.checkSelectableProducts(receivedMoney)

          println("received Money: " + receivedMoney)

          vendingMachine.showBuyableProducts(receivedMoney)


          println("\n상품을 선택하세요.")
          val selectionOfProduct = scala.io.StdIn.readInt()

          vendingMachine.selectToBuyProduct(receivedMoney, selectionOfProduct)

          if (vendingMachine.isLackOfMoney) {

            println("돈을 추가 하시겠습니까? (Y/N)")
            println("-------------------------------------------")

            val selectToAddMoney = scala.io.StdIn.readLine

            vendingMachine.askAdditionalMoney(receivedMoney, selectToAddMoney)

            receivedMoney = receivedMoney + vendingMachine.additionalMoney

            vendingMachine.isLackOfMoney = false
          }

          else{
            vendingMachine.askContinueBuying(vendingMachine.output)
            vendingMachine.hereIsYourChange(vendingMachine.selectToReturnChange)
            receivedMoney = vendingMachine.output
          }

        }
          vendingMachine.selectionOfMenu = true
      }
    }
  }
}

