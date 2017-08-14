package vendingMachineEx


class MachineController {

  def control: Unit = {

    val vendingMachine = new VendingMachine

    while (true) {

      vendingMachine.inputCheck(vendingMachine.inputMoney())


    }
  }
}