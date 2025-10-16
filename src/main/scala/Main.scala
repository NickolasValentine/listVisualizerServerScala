import server.HttpServerApp

object Main {
  @throws[Exception]
  def main(args: Array[String]): Unit = {
    HttpServerApp.start()
  }
}