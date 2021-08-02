package module4.phoneBook.services

import zio.Has
import zio.Task
import module4.phoneBook.dto._
import zio.{ZLayer, ULayer}
import zio.ZIO
import zio.RIO
import zio.IO
import zio.macros.accessible
import java.sql.DriverManager
import module4.phoneBook.dao.repositories.PhoneRecordRepository
import doobie.free.resultset
import module4.phoneBook.db.DBTransactor
import zio.interop.catz._
import module4.phoneBook.dao.entities.PhoneRecord
import zio.random.Random

@accessible
object PhoneBookService {

     type PhoneBookService = Has[Service]
  
     trait Service{
       def find(phone: String): ZIO[DBTransactor, Option[Throwable], (String, PhoneRecordDTO)]
       def insert(phoneRecord: PhoneRecordDTO): RIO[DBTransactor with Random, String]
       def update(id: String, phoneRecord: PhoneRecordDTO): RIO[DBTransactor, Unit]
       def delete(id: String): RIO[DBTransactor, Unit]
     }

    class Impl(phoneRecordRepository: PhoneRecordRepository.Service) extends Service {
      import doobie.implicits._
        def find(phone: String): ZIO[DBTransactor, Option[Throwable], (String, PhoneRecordDTO)] = for{
          transactor <- DBTransactor.dbTransactor
          result <- phoneRecordRepository.find(phone).transact(transactor).some
        } yield (result.id, PhoneRecordDTO.from(result))

        def insert(phoneRecord: PhoneRecordDTO): RIO[DBTransactor with Random, String] = for{
          transactor <- DBTransactor.dbTransactor
          uuid <- zio.random.nextUUID.map(_.toString())
          _ <- phoneRecordRepository.insert(PhoneRecord(uuid, phoneRecord.phone, phoneRecord.fio))
                  .transact(transactor)
        } yield uuid
        
        def update(id: String, phoneRecord: PhoneRecordDTO): RIO[DBTransactor, Unit] = for{
            transactor <- DBTransactor.dbTransactor
            _ <- phoneRecordRepository.update(PhoneRecord(id, phoneRecord.phone, phoneRecord.fio)).transact(transactor)
        } yield ()
        
        def delete(id: String): RIO[DBTransactor, Unit] = for{
            transactor <- DBTransactor.dbTransactor
            _ <- phoneRecordRepository.delete(id).transact(transactor)
        } yield ()
        
    }

    val live: ZLayer[PhoneRecordRepository.PhoneRecordRepository, Nothing, PhoneBookService.PhoneBookService] = 
      ZLayer.fromService[PhoneRecordRepository.Service, PhoneBookService.Service](repo => new Impl(repo))

}
