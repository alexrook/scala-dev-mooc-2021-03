package module3

import zio._
import zio.clock.Clock
import zio.console._
import zio.duration._
import zio.macros.accessible
import zio.random.Random

import java.util.concurrent.TimeUnit
import scala.language.postfixOps

package object zio_homework {

  import config._
  import zioConcurrency._

  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в когнсоль угадал или нет.
   */

  lazy val guessProgram: URIO[Console with Random, Unit] = for {
    rnd <- random.nextIntBetween(1, 3)
    _ <- console.putStrLn("Enter number between 1 to 3")
    guess <- console.getStrLn.flatMap(str =>
      ZIO.effect(str.toInt)
    ).either
    _ <- guess match {
      case Right(value) =>
        if (rnd == value) {
          console.putStrLn("You are winner")
        } else {
          console.putStrLn(s"You are lost, rnd = $rnd")
        }
      case Left(_) =>
        console.putStrLn("Incorrect input, try again") *> guessProgram
    }
  } yield ()


  /**
   * 2. реализовать функцию doWhile, которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   */
  def doWhile[R, E, A](body: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] = body.flatMap { a: A =>
    if (condition(a)) {
      ZIO.effectTotal(a)
    } else {
      doWhile(body)(condition)
    }
  }

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из файла, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "load" из пакета config
   */

  def loadConfigOrDefault: ZIO[Console, Nothing, AppConfig] =
    for {
      cfg <- load.orElse(ZIO.effectTotal(AppConfig("default-name", "example.com")))
      _ <- putStrLn(cfg.toString)
    } yield cfg


  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайеым образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: URIO[Random with Clock, Int] =
    ZIO.sleep(Duration.apply(1, TimeUnit.SECONDS)) *> random.nextIntBetween(0, 10)

  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: List[URIO[Random with Clock, Int]] = List.fill(10)(eff)

  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекци "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: URIO[Console with Clock with Random, Int] =
    printEffectRunningTime(ZIO.collectAll(List.fill(10)(eff)).map(_.sum))

  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val speedUp: URIO[Random with Clock, Int] = ZIO.collectAllPar(List.fill(10)(eff)).map(_.sum)

  lazy val appSpeedUp: URIO[Console with Clock with Random, Int] = printEffectRunningTime(speedUp)


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * молжно было использовать аналогично zio.console.putStrLn например
   */

  @accessible
  object EffectRunningTime {

    type EffectRunningTime = Has[EffectRunningTime.Service]

    trait Service {
      def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[Console with Clock with R, E, A] =
        zioConcurrency.printEffectRunningTime(zio)
    }

    val live: ULayer[EffectRunningTime] = ZLayer.succeed(new EffectRunningTime.Service {})
  }

}

object HomeWorkTestApp extends zio.App {

  import zio_homework._
  import EffectRunningTime._

  //  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
  //    guessProgram *> ZIO.succeed(ExitCode(0))

  //  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
  //    loadConfigOrDefault *> ZIO.succeed(ExitCode(0))


  //  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
  //    console.getStrLn.map(_.toInt).either.flatMap {
  //      case Left(value) =>
  //        console.putStrLn("Boom")
  //      case Right(value) =>
  //        console.putStrLn(value.toString)
  //    } *> ZIO.effectTotal(ExitCode(0))

  //      override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
  //        app.exitCode

  //    override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
  //      appSpeedUp.flatMap(s => console.putStrLn(s.toString)).exitCode

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    val effect: URIO[Console with Clock with Random, Int] = speedUp

    val program =
      for {
        e <- EffectRunningTime.printEffectRunningTime(effect)
      } yield ()

    program
      .provideSomeLayer[Console with Clock with Random](EffectRunningTime.live).exitCode

  }

}
