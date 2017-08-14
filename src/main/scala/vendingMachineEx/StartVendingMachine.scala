package vendingMachineEx


object StartVendingMachine {

  def main(args: Array[String]): Unit = {

    val VendingMachine = new MachineController

    while (true) {

      VendingMachine.inputCheck(VendingMachine .inputMoney()) // 돈을 넣지않고 메뉴 선택하면 경고
    }
  }
}


