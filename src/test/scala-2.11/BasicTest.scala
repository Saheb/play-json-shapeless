import org.scalatest.FlatSpec
import play.api.libs.json._
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness, _}
import labelled._
import shapeless.labelled.FieldType

trait BaseWrites extends DefaultWrites

object AutoWrites extends BaseWrites {

//  implicit val personFormat: Format[Person] = new Format[Person] {
//
//    override def writes(o: Person) = ???
//
//    override def reads(json: JsValue) = ???
//  }

  def createFormat[A](writerFn: A => JsValue, readerFn: JsValue => JsResult[A]): Format[A] = new Format[A] {
    override def writes(o: A): JsValue = writerFn(o)

    override def reads(json: JsValue): JsResult[A] = readerFn(json)
  }

//  Already exists
//  implicit def listFormat[A](implicit fmt: Format[A]): Format[List[A]]  = createFormat(list => JsArray(list.map(fmt.writes(_))), json => json match {
//    case JsArray(elems) =>
//      JsSuccess(elems.map(fmt.reads(_)).toList.map(_.get))
//  })

  implicit def hNilFormat: Format[HNil] = createFormat(hnil => JsObject(Nil), json => JsSuccess(HNil))

  implicit def hListFormat[K <: Symbol, H, T <: HList](implicit
                                                        witness: Witness.Aux[K],
                                                        hFormat: Lazy[Format[H]],
                                                        tFormat: Format[T]): Format[FieldType[K, H] :: T] = {
    val fieldName = witness.value.name
    createFormat (
      hlist => {
          val head: JsValue = hFormat.value.writes(hlist.head)
          val tail: JsValue = tFormat.writes(hlist.tail)
          JsObject((fieldName, head) :: tail.asInstanceOf[JsObject].fields.toList) // Remove boxing
        },
      json => {
        json match {
          case obj: JsObject =>
            obj.value.get(fieldName) match {
              case Some(jsValue) =>
                val head = hFormat.value.reads(jsValue)
                val tail = tFormat.reads(obj)
                //head.map(h => field[K](h) :: tail.get)
                println(tail.get)
                JsSuccess(field[K](head.get) :: tail.get)
              case None =>
               ???
            }
          case _ =>
            ???
        }
//          val head: JsResult[H] = hFormat.value.reads(json)
//          println(head)
//          val tail: JsResult[T] = tFormat.reads(json)
//          println(tail)
//          val tmp: JsResult[::[(K, H), T]] = tail.map(t => (witness.value, head.get) :: t)
//          tmp.asInstanceOf[JsResult[FieldType[K, H] :: T]]
        }
    )
  }

  implicit def genFormat[T, H <: HList](implicit gen: LabelledGeneric.Aux[T, H], hFormat: Lazy[Format[H]]): Format[T] = new Format[T] {

    override def writes(o: T) = {
      val t: gen.Repr = gen.to(o)
      hFormat.value.writes(t)
    }

    override def reads(json: JsValue): JsResult[T] = {
      val t: JsResult[H] = hFormat.value.reads(json)
      JsSuccess(gen.from(t.get))
    }
  }
}

class BasicTest extends FlatSpec {

  case class Person(name: String)
  case class Foo(f: Boolean, t: String)
  case class Company(employees: List[Person])
  import AutoWrites._

  "Json Format" should "be able to convert to Json String" in {
    val p = Person("Bob")
    //val json: JsValue = Json.toJson(Map("key" -> 10))
    //val p2 = Json.fromJson(json).asOpt.get.copy( name= "John")
    assert(Json.toJson(p).toString() == """{"name":"Bob"}""")
    assert(Json.fromJson[Person](Json.toJson(p)).get == p)
    assert(Json.fromJson[List[Foo]](Json.toJson(List(Foo(false, "hello"), Foo(true, "hey")))).get == List(Foo(false, "hello"), Foo(true, "hey")))
    assert(Json.fromJson[Company](Json.toJson(Company(List(Person("Sam"))))).get == Company(List(Person("Sam"))))
  }
}
