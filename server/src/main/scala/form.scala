import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue

import spray.json._
import spray.json.DefaultJsonProtocol
import spray.json.DeserializationException
import spray.json.JsArray
import spray.json.JsNumber
import spray.json.JsObject
import spray.json.JsString
import spray.json.JsValue
import spray.json.JsonFormat
import spray.json.pimpAny
import java.util.concurrent.atomic._

import java.security._


object jsonform extends DefaultJsonProtocol {
	case class UserInfo(firstname:String,lastname:String,gender:String,age:Int,email:String)
	case class Response(status:String, id:String,message:String)
	implicit val userInfoFormat = jsonFormat5(UserInfo)
	implicit val responseFormat = jsonFormat3(Response)
}


//App
case class start(port:Int)   


//Client
case class Initialize(id:Int)
case object Sendpost
case class Readpost(id:Int)
case class Downloadpost(target_id:Int,post:ConcurrentHashMap[String, Array[Byte]],en_AES:String)

//Server
case class Adduser(id:Int,gender:String,firstname:String,lastname:String,age:Int,email:String,publickey:PublicKey)

case class Getuser(id:Int)

case class askpubkey(id:Int)

case class Uploadpost(id:Int,content: Array[Byte],vi:String)

case class Sendingpost(id:Int,target_id:Int) 

case class newfrnd_encode_AES(id:Int,friendid:Int,en_AES:String)

case class verifyme(id:Int)

case class checksign(id:Int,sign:String)