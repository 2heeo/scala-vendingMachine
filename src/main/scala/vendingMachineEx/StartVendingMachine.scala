package vendingMachineEx


object StartVendingMachine {

  def main(args: Array[String]): Unit = {

    val vendingMachine = new MachineController

    vendingMachine.control

  }

}


