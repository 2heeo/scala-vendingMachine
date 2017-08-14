package vendingMachineEx


class MachineController {

  def control: Unit = {

    val test = new Test

    while (true) {

    test.inputCheck(test.inputMoney()) // 처음 시작화면 출력, 돈 투입 시도 후 투입된 값이 돈인지 확인

    }
  }
}