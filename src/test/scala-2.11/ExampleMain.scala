import AutoWrites._
import play.api.libs.json.Json

case class Bar(name: String, location: String)
case class Resource(id: String, available: Boolean)
case class Reservation(id: String, resource: List[Resource])

object ExampleMain extends App {

  val id = java.util.UUID.randomUUID().toString
  println(Json.toJson(Bar("ABC", "Manchester")))
  println(Json.toJson(Resource(id, true)))
  println(Json.toJson(List(Resource(id, true))))
  println(Json.toJson(Reservation(java.util.UUID.randomUUID().toString, List(Resource(id, false)))))

  println(Json.fromJson[Bar](Json.toJson(Bar("ABC", "Manchester"))))
  println(Json.fromJson[List[Resource]](Json.toJson(List(Resource(id, true)))))
  println(Json.fromJson[Reservation](Json.toJson(Reservation(java.util.UUID.randomUUID().toString, List(Resource(id, false))))))
}
