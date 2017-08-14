package vendingMachineEx


class MachineController {

  def control: Unit = {

    val test = new Test

    while (true) {

      test.inputCheck(test.inputMoney())


    }
  }
}