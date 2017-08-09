import scala.io.StdIn
import scala.util.Random


object VendingMachine {

  def main(args: Array[String]): Unit = {

    val Repository = new repository

    while (true) {

      Repository.printIntro() // 시작 화면 출력

      var money = Repository.moneyCheck(Repository.inputMoney()) // 돈을 넣지않고 메뉴 선택하면 경고

    }
    println("-------------------------------------------")
  }

}

