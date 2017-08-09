package VendingMachineEx


object VendingMachine {

  def main(args: Array[String]): Unit = {

    val Repository = new repository

    while (true) {

      Repository.moneyCheck(Repository.inputMoney()) // 돈을 넣지않고 메뉴 선택하면 경고
    }
  }

}

