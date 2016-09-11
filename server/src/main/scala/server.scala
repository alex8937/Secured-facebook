
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic._
import java.net.InetAddress
import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.io.IO
import spray.can.Http
import spray.http._
import spray.http.{HttpMethods, HttpRequest, HttpResponse, Uri}
import spray.http.HttpMethods._
import akka.routing.SmallestMailboxPool
import akka.routing.RoundRobinPool
import akka.event.LoggingReceive
import com.typesafe.config.ConfigFactory

import scala.collection.mutable.ArrayBuffer
import com.roundeights.hasher.Implicits._
import scala.language.postfixOps

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

import java.security._
import java.security.SecureRandom


class ServerController extends Actor{

    var cores = (Runtime.getRuntime().availableProcessors()).toInt
    
	val router =context.actorOf(SmallestMailboxPool(cores*2).props(Props[ServerActor]),"Router")  //Set up a router to assign tasks    


    println("Num of router: "+cores)
    
    println("router  "+router)
    def receive = {
		case "remote"=>println(self+"  remote")
		case "newuser"=>println("newuser")
		case _ =>{
			println("ServerController here")
			router ! "hi"
				 } 
	}

    
}

object common {
	var userstore: ConcurrentHashMap[String, User] = new ConcurrentHashMap()
	var random_store : ConcurrentHashMap[Int,  Unit] = new ConcurrentHashMap()
}


case class User (id: Int, gen:String,fname: String, lname: String, age:Int,email: String,pubkey:PublicKey) {
	var userId = id
	var gender=gen
	var firstname = fname
	var lastname = lname
	var age0=age
	var emailAdd = email
	var publickey=pubkey

	var posts : ConcurrentHashMap[String,  Array[Byte]] = new ConcurrentHashMap()
	
	var friends: ConcurrentHashMap[Int, String] = new ConcurrentHashMap()
	var friendRequests: ConcurrentHashMap[String, String] = new ConcurrentHashMap()
	var likedpages: ConcurrentLinkedQueue[String] = new ConcurrentLinkedQueue()
}

class ServerActor extends Actor {
	import jsonform._

	import common._
  //lazy val serverController = context.actorSelection("//ServerSystem/user/server_controller")

	def receive = {
		//case "remote"=>println(self+"  remote")

			
		case Adduser(id,gender,firstname,lastname,age,email,pubkey) =>{
			userstore.put(id+"", new User(id,gender,firstname,lastname,age,email,pubkey))
			println("User "+id+" registered");
			//sender ! Response("SUCCESS",id+"","").toJson.toString
		}
			
		case Getuser(id) =>	{		
			if(userstore.containsKey(id.toString)){
				var obj=userstore.get(id.toString)
				var temp=UserInfo(obj.fname,obj.lname,obj.gen,obj.age,obj.email)
                sender ! temp.toJson.toString
                println("user"+id)	
			}
			else{
				var temp="User "+id+" Not exist"
				sender ! temp.toJson.toString
				println(temp)	
			}
		}

		case Uploadpost(id,enc_post,postid) =>{
			var obj=userstore.get(id.toString)
			obj.posts.put(postid,enc_post)
		}

		case Sendingpost(id,target_id) =>{
			if(userstore.containsKey(target_id.toString)){
				var obj=userstore.get(target_id.toString)
				var posts=obj.posts
				if(obj.friends.containsKey(id)){
					var en_AES=obj.friends.get(id)
					sender ! Downloadpost(target_id,posts,en_AES)
				}
				else 
				{
					sender ! "Msg_not_friend"
				}
				
			}else{
				var temp="User "+target_id+" Not exist"
				println(temp)
			}
		}


		case verifyme(id)=>{
			val rand = new SecureRandom();
			var nbyte: Array[Byte]= Array(1024.toByte);
			var r=rand.nextBytes(nbyte)
			//var r_hash=r.sha256.hex
			random_store.put(id,r)
			sender ! r
		}
		case checksign(id,sign)=>{
			var obj=userstore.get(id.toString)
			var publickey=RSA.encodePublicKey(obj.pubkey)
			var sign2=RSA.decrypt(sign,publickey)
			//var sign2hash=sign2.sha256.hex
			var store=random_store.get(id)
			if(store.toString==sign2){
				sender ! true
			}
			else {
				sender ! false
			}
		}



		case askpubkey(id) => {
			if(userstore.containsKey(id.toString)){
				var obj=userstore.get(id.toString)
				var temp=RSA.encodePublicKey(obj.pubkey)
                sender ! temp	
			}
			else{
				var temp="User "+id+" Not exist"
				sender ! temp.toJson.toString
				println(temp)	
			}
		}

		case newfrnd_encode_AES(id,friendid,en_AES) =>{
			if(userstore.containsKey(id.toString)&&userstore.containsKey(friendid.toString)){
				var obj=userstore.get(id.toString)
				var temp=obj.friends
                temp.put(friendid,en_AES)
                //if(id==3&&friendid==4) println(en_AES)
				//println(id+"   "+friendid+"    "+en_AES)				
			}
			else{
				var temp="User "+id+" Not exist"
				println("newfrnd_encode_AES case err:")
				println(temp)
			}			
		}



		case _ =>{ 
			println("Server " + self)			
    	}
  }
}



object Main {

  def main(args: Array[String]) {
  	val serverSystemName: String = "my-server-system"
    val config = ConfigFactory.load()
    var serverCustomConfig: String = null

    try {
      serverCustomConfig = """
		akka {
		  actor {
		    provider = "akka.remote.RemoteActorRefProvider"
		  }
		  remote {
		    netty.tcp {
		      hostname = "127.0.0.1"
		      port = 1234
		      
		      send-buffer-size = 8192000b
		      receive-buffer-size = 8192000b
		      maximum-frame-size = 4096000b
		    }
		  }
		}
		"""
    } catch {
      case ex: Exception => {
        serverCustomConfig = null
      }
    }

    val serverConfig = config.getConfig(serverSystemName)
    var system: ActorSystem = null
    if (null != serverCustomConfig) {
      val customConf = ConfigFactory.parseString(serverCustomConfig);
      system = ActorSystem(serverSystemName, ConfigFactory.load(customConf).withFallback(serverConfig))
    } else {
      system = ActorSystem(serverSystemName, serverConfig.withFallback(config))
    }



    //val serverConfig = ConfigFactory.load().getConfig("my-server-system")
    //println(serverConfig)

    val serverctr: ActorRef=system.actorOf(Props[ServerController], "ServerController")

    println("serverctr "+serverctr)

    serverctr ! "Hi"
  }

}

