package vendingMachineEx


class MachineController {

  def control: Unit = {

    val product = new Product

    val vendingMachine = new VendingMachine(product)

    while (true) {

      vendingMachine.inputCheck(vendingMachine.inputMoney())


    }
  }
}