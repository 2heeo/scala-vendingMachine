package VendingMachineEx


object VendingMachine {

  def main(args: Array[String]): Unit = {

    val VendingMachine = new machineController

    while (true) {

      VendingMachine.moneyCheck(VendingMachine .inputMoney()) // 돈을 넣지않고 메뉴 선택하면 경고
    }
  }

}

