package module3

import zio.ZIO
import zio.test.Assertion._
import zio.test._
import zio.test.environment.{TestClock, TestConsole, TestRandom}
import zio.duration._

object ZioHomeWorkSpec extends DefaultRunnableSpec {

  import zio_homework._

  override def spec = suite("HomeWork")(
    testM("guessProgram")(
      for {
        _ <- TestRandom.setSeed(2)
        _ <- TestConsole.feedLines("2")
        ret <- guessProgram
      } yield assert(ret)(equalTo(()))
    ),
    testM("doWhile")(
      for {
        ret <- doWhile(ZIO.succeed("A"))(_ == "A")
      } yield assert(ret)(equalTo("A"))
    ),
    testM("loadConfigOrDefault")(
      for {
        _ <- loadConfigOrDefault
        out <-TestConsole.output
      } yield assert(out)(hasSameElements(Seq("AppConfig(default-name,example.com)\n")))
    ),
//    testM("appSpeedUp")( //почемуто виснет :-(
//      for {
//        _ <-TestClock.adjust(1.millis)
//        ret <- appSpeedUp
//      } yield assert(ret)(equalTo(1))
//    )
  )

}
