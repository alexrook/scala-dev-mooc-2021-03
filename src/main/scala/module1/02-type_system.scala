package module1

import java.io.Closeable

object type_system {

  /**
   * Scala type system
   *
   */

   // AnyVal

   //

   // Unit



   // Null


   // Nothing

  def absurd(v: Nothing) = ???


  // Generics

    def ensureClose[T <: Closeable](o: T)(action: T => Unit): Unit = ???



  /**
   *
   * class
   *
   *
   */




  /**
   * Задание 1: Создать класс "Прямоугольник"(Rectangle), мы должны иметь возможность создавать прямоугольник с заданной
   * длиной(length) и шириной(width), а также вычислять его периметр и площадь
   */



  /**
   * object
   *
   * 1. Паттерн одиночка
   * 2. Линивая инициализация
   */






  /**
   * case class
   *
   */

   // создать case класс кредитная карта с двумя полями номер и cvc






  /**
   * case object
   *
   * Используются для создания перечислений или же в качестве сообщений для Акторов
   */

    trait Color
    case object Red extends Color
    case object Green extends Color
    case object Blue extends Color


  /**
   * trait
   *
   */




  class A {
    def foo() = "A"
  }

  trait B extends A {
    override def foo() = "B" + super.foo()
  }

  trait C extends B {
    override def foo() = "C" + super.foo()
  }

  trait D extends A {
    override def foo() = "D" + super.foo()
  }

  trait E extends C {
    override def foo(): String = "E" + super.foo()
  }





  /**
   * Value classes и Universal traits
   */


}
