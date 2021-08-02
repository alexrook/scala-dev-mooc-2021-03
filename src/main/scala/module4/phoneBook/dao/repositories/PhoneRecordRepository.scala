package module4.phoneBook.dao.repositories

import module4.phoneBook.db.DBTransactor
import zio.Has
import module4.phoneBook.dao.entities._
import doobie.quill.DoobieContext
import io.getquill.CompositeNamingStrategy2
import io.getquill.Escape
import io.getquill.Literal
import zio.ZLayer
import zio.ULayer

object PhoneRecordRepository {
  val dc: DoobieContext.Postgres[CompositeNamingStrategy2[Escape.type, Literal.type]] = DBTransactor.doobieContext
  import dc._

  type PhoneRecordRepository = Has[Service]

  trait Service{
      def find(phone: String): Result[Option[PhoneRecord]]
      def list(): Result[List[PhoneRecord]]
      def insert(phoneRecord: PhoneRecord): Result[Unit]
      def update(phoneRecord: PhoneRecord): Result[Unit]
      def delete(id: String): Result[Unit]
  }

  class Impl extends Service{

    val phoneRecordSchema = quote{
        querySchema[PhoneRecord](""""PhoneRecord"""")
    }
    
   def find(phone: String): Result[Option[PhoneRecord]] = 
    dc.run(phoneRecordSchema.filter(_.phone == lift(phone))).map(_.headOption) // SELECT "x1"."id", "x1"."phone", "x1"."fio" FROM "PhoneRecord" "x1" WHERE "x1"."phone" = ?
   
   def list(): Result[List[PhoneRecord]] = dc.run(phoneRecordSchema) // SELECT "x"."id", "x"."phone", "x"."fio" FROM "PhoneRecord" "x"
   
   def insert(phoneRecord: PhoneRecord): Result[Unit] = dc.run(phoneRecordSchema.insert(lift(phoneRecord))).map(_ => ())
   
   def update(phoneRecord: PhoneRecord): Result[Unit] = dc.run(phoneRecordSchema.filter(_.id == lift(phoneRecord.id))
   .update(lift(phoneRecord))).map(_ => ()) // UPDATE "PhoneRecord" SET "id" = ?, "phone" = ?, "fio" = ? WHERE "id" = ?
   
   def delete(id: String): Result[Unit] = dc.run(phoneRecordSchema.filter(_.id == lift(id)).delete).map(_ => (()))

   def foo() = {
      
     val a = dc.run(
         phoneRecordSchema
         .filter(_.phone == lift("1234"))
         .filter(_.fio == lift("fio"))
         ) // SELECT "x8"."id", "x8"."phone", "x8"."fio" FROM "PhoneRecord" "x8" WHERE "x8"."phone" = ? AND "x8"."fio" = ?

      val b = dc.run(phoneRecordSchema.filter(_.id == lift("1")))
      
      val c = for{
             r1 <- a
             r2 <- b
         } yield (r2)

    //   val r1 = quote{
    //       for{
    //           q1  <- phoneRecordSchema.filter( r => r.id == lift("dddd") || r.fio == lift("vfvfvfvf"))
    //           q2  <- addressSchema.join(_.id == q1.addressId)
    //       } yield (q2.fio)
    //   }

    //   dc.run(r1) // SELECT "x11"."fio" FROM "PhoneRecord" "r" INNER JOIN "PhoneRecord" "x11" ON "x11"."id" = "r"."id" WHERE "r"."id" = ? OR "r"."fio" = ?

   }
   
      
  }

  val live: ULayer[PhoneRecordRepository] = ZLayer.succeed(new Impl())
}
