package vendingMachineEx


object StartVendingMachine {

  def main(args: Array[String]): Unit = {

    val machineController = new MachineController

    machineController.control

  }

}


