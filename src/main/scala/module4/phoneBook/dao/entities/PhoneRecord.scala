package module4.phoneBook.dao.entities

import fansi.Str

case class PhoneRecord(id: String, phone: String, fio: String)

case class Address(id: String, zipCode: String, streetAddress: String)