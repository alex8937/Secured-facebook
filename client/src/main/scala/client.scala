import akka.actor._
import akka.util.Timeout
import akka.pattern.ask

import akka.routing.RoundRobinPool
import scala.io.StdIn.readLine
import scala.io.Source
import scala.util.Random
import scala.util.Success
import scala.util.Failure
import scala.concurrent.Await
import scala.concurrent.duration._

import com.roundeights.hasher.Implicits._
import scala.language.postfixOps

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

import spray.client.pipelining.WithTransformerConcatenation
import spray.client.pipelining.sendReceive
import spray.client.pipelining.sendReceive$default$3
import spray.client.pipelining.unmarshal
import spray.http.ContentTypes
import spray.http.HttpEntity
import spray.http.HttpMethods.GET
import spray.http.HttpMethods.POST
import spray.http.HttpRequest
import spray.http.HttpResponse
import spray.http.Uri.apply
import spray.json.pimpAny
import spray.json.pimpString

import scala.collection.mutable.ArrayBuffer

import spray.json.DefaultJsonProtocol
import spray.json.DeserializationException
import spray.json.JsArray
import spray.json.JsNumber
import spray.json.JsObject
import spray.json.JsString
import spray.json.JsValue
import spray.json.JsonFormat
import spray.json.pimpAny

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentHashMap

import java.security._




object common {

	val malenamelist = Source.fromFile("male.txt").getLines.toList
	val femalenamelist=Source.fromFile("female.txt").getLines.toList
	val lastnamelist=Source.fromFile("lastname.txt").getLines.toList
	val typelist = List("person", "group", "event")
	val genderlist= List("male","female")
	var counter:AtomicInteger = new AtomicInteger();
	var postcounter:AtomicInteger = new AtomicInteger(); 
    var readcounter:AtomicInteger = new AtomicInteger(); 
	var friends: ConcurrentHashMap[Int, String] = new ConcurrentHashMap() //Store friendpublickey hash

}





class LocalActor (system: ActorSystem,nrofuser:Int) extends Actor {

	import common._
	import jsonform._
	var ifcomplete=false
    var ip="127.0.0.1"
  	var id=counter.addAndGet(1)
    var firstname:String=""
    var lastname:String=""
    var email:String=""
    var age:Int=0
    var nroffriends=0
    var type_id:Int=0
    var postfreq=0
    var readfreq=0
    var if_verify:Boolean=false

    val keypair = RSA.generateKey
    val publickey = keypair.getPublic
    val privatekey = keypair.getPrivate
    val AESkey=AES.generateAESKey

    var numcomplete=0
    //if(id==3) println(id+"---------"+publickey)

    val r = new Random
    var gender=genderlist(r.nextInt(genderlist.length)).toString
    if(gender=="male") firstname=malenamelist(r.nextInt(malenamelist.length)).toString
    if(gender=="female") firstname=femalenamelist(r.nextInt(femalenamelist.length)).toString
    lastname=lastnamelist(r.nextInt(lastnamelist.length)).toString
    email=firstname+"_"+lastname+"@facebook.com"
    age=rand_age
    nroffriends=rand_num_friend(age)
    type_id=rand_type
    while(postfreq<=0){
        postfreq=Math.ceil(rand_post_fre(type_id)/100).toInt
    }
    while(readfreq<=0){
        readfreq=Math.ceil(rand_post_fre(type_id)/1000).toInt
    }
    
    var post_last:Int=0
    var read_last:Int=0

    val server=context.actorSelection("akka.tcp://my-server-system@"+ip+":1234/user/ServerController/Router")
    val http=context.actorSelection("akka.tcp://my-http-system@"+ip+":3000/user/app")

    var run1 :Cancellable=null
    var postrun :Cancellable=null
    var readrun :Cancellable=null
	
    
	def rand_age :Int={  //http://www.statista.com/statistics/376128/facebook-global-user-age-distribution/
		val r = new Random
		val ran1=r.nextInt(100)+1
		var ran2=0
		if(ran1<=25){ran2=r.nextInt(9)+16}
		else if(ran1>25&&ran1<=(25+28)){ran2=r.nextInt(9)+16}
		else if(ran1>(25+28)&&ran1<=(25+28+22)){ran2=r.nextInt(10)+25}
		else if(ran1>(25+28+22)&&ran1<=(25+28+22+16)){ran2=r.nextInt(10)+35}
		else if(ran1>(25+28+22+16)&&ran1<=(25+28+22+16+9)){ran2=r.nextInt(10)+45}
		else {ran2=r.nextInt(10)+55}
		return ran2
	}

    def rand_num_friend(ages :Int) : Int={
        val r = new Random
        var ran2=0
        if(ages<=17){ran2=(Math.ceil(r.nextGaussian()*Math.sqrt(52)+521)).toInt}
        else if(ages>=18&&ages<=24){ran2=Math.ceil(r.nextGaussian()*Math.sqrt(649)+649).toInt}
        else if(ages>=25&&ages<=34){ran2=Math.ceil(r.nextGaussian()*Math.sqrt(360)+360).toInt}
        else if(ages>=35&&ages<=44){ran2=Math.ceil(r.nextGaussian()*Math.sqrt(277)+227).toInt}
        else if(ages>=45&&ages<=54){ran2=Math.ceil(r.nextGaussian()*Math.sqrt(220)+220).toInt}
        else if(ages>=55&&ages<=64){ran2=Math.ceil(r.nextGaussian()*Math.sqrt(129)+129).toInt}
        else {ran2=Math.ceil(r.nextGaussian()*Math.sqrt(102)+102).toInt}

        if(ran2>=nrofuser) {ran2=nrofuser-1}

        return ran2
    }

    def Initialize_friend(numoffriend: Int) {
        friends.clear()
    	while(friends.size()<=numoffriend){

    		val r = new Random
    		val ran=r.nextInt(nrofuser)+1
            val friendid=ran
    		if(ran!=id){
		    	sendnewfriend_encode_AES(friendid)
		    	implicit val timeout = Timeout(5 seconds)
		    	val future = server ? askpubkey(friendid)
		    	val result = Await.result(future, timeout.duration).asInstanceOf[String] //Friend's publickey
		    	var hashvalue = result.sha256.hex
		    	friends.put(friendid,hashvalue)
    		}
    	}
    }

    def rand_type :Int={  //http://www.statista.com/statistics/376128/facebook-global-user-age-distribution/
        val r = new Random
        val ran1=r.nextInt(100)+1
        var ran2=0
        if(ran1<=23){ran2=1}
        else if(ran1>23&&ran1<=(23+29)){ran2=2}
        else if(ran1>(23+29)&&ran1<=(23+29+26)){ran2=3}
        else if(ran1>(23+29+26)&&ran1<=(23+29+26+17)){ran2=4}
        else {ran2=5}
        return ran2
    }

    def rand_post_fre(tp:Int) :Int={
        var num=0
        var conv=3600000
        val r = new Random
        if(tp==1){num=Math.ceil(r.nextGaussian()*0.3*conv*0.01+0.3*conv).toInt}
        else if(tp==2) {num=Math.ceil(r.nextGaussian()*8*conv*0.8+8*conv).toInt}
        else if(tp==3) {num=Math.ceil(r.nextGaussian()*48*conv*0.8+48*conv).toInt}
        else if(tp==4) {num=Math.ceil(r.nextGaussian()*48*5*conv*0.8+48*5*conv).toInt}
        else {num=Math.ceil(r.nextGaussian()*48*50*conv*0.8+48*50*conv).toInt}
        return num
    }

    def sendnewfriend_encode_AES(friendid:Int) {
    	implicit val timeout = Timeout(5 seconds)
    	val future = server ? askpubkey(friendid)
    	val frnd_pubkey = Await.result(future, timeout.duration).asInstanceOf[String] 
 		  var en_AES=RSA.encrypt(AES.AEStoString(AESkey),frnd_pubkey)
 		  server ! newfrnd_encode_AES(id,friendid,en_AES)
 		//println(id+"   "+friendid+"    "+en_AES)
    }

    def verify(id:Int):Boolean={
        implicit val timeout = Timeout(5 seconds)
        
        val future = server ? verifyme(id)
        val result = Await.result(future, timeout.duration).asInstanceOf[Unit] 
        if(id==3) println(result)
        var rad_str=result.toString
        var sign:String=""
        sign=RSA.encrypt(rad_str,privatekey)
        val future2=server ? checksign(id,sign)
        val result2 = Await.result(future2, timeout.duration).asInstanceOf[Boolean] 
        return result2
    }
    
    def receive={
    
    	case Initialize	=>{
    		val r = new Random
    		var gender=genderlist(r.nextInt(genderlist.length)).toString
    		if(gender=="male") firstname=malenamelist(r.nextInt(malenamelist.length)).toString
    		if(gender=="female") firstname=femalenamelist(r.nextInt(femalenamelist.length)).toString
    		lastname=lastnamelist(r.nextInt(lastnamelist.length)).toString
    		email=firstname+"_"+lastname+"@facebook.com"
    		server ! Adduser(id,gender,firstname,lastname,age,email,publickey)
            var postid=0.toString
            var postcontent: String="Try first post"
            var enc_post=AES.encryptAES(AESkey, postcontent, postid)

            Thread.sleep(300);

            server ! Uploadpost(id,enc_post,postid)

    		run1=system.scheduler.schedule(0 milliseconds,5 seconds, self, "Start")

    		/*
    		val pipeline: HttpRequest => Future[String] = sendReceive ~> unmarshal[String]
			val request = HttpRequest(method = GET, 
    						uri = "http://127.0.0.1:8080/people", 
    						entity = HttpEntity(ContentTypes.`application/json`, 
    						UserInfo(id,gender,firstname,lastname,email).toJson.toString))
    		
    		val responseFuture: Future[String] = pipeline(request)
    			responseFuture onComplete {
    				case Success(result) =>
    					var resp = result.parseJson.convertTo[Response]
    					println("Account Creation: " + resp.status + ": " + resp.message)
    					if(resp.status == "SUCCESS"){
    						server ! Adduser(id,gender,firstname,lastname,email)
    						println("User " + id + " created!!!")
    				}
    				case Failure(error) =>
    					println("User "+id+" Creation: " + error)
    			}	
			*/
    	}
    	case Sendpost => {

    		var postid=postcounter.addAndGet(1).toString
            //println("sendpost",id,postid)
			  var postcontent: String="Upload post "+postid
			  var enc_post=AES.encryptAES(AESkey, postcontent,postid)
			  server ! Uploadpost(id,enc_post,postid)
    	}

    	case Readpost (id)=> {
            var target_id=0
            val r = new Random
            var v=r.nextInt(nroffriends-2)
            var pool=friends.keySet.toArray
            target_id=r.nextInt(nrofuser)+1//pool(v).asInstanceOf[Int]
    		val future=server ! Sendingpost(id,target_id)
    	}
    	case Downloadpost(target_id,post,target_en_AES)=>{
            var frnd_AES_str=RSA.decrypt(target_en_AES,privatekey)
            var frnd_AES=AES.StringtoAES(frnd_AES_str)
            var postidpool=post.keys
            while(postidpool.hasMoreElements){
                var postid=postidpool.nextElement()
                if(postid.toInt!=0) {
                    var de_post=AES.decryptAES(post.get(postid),frnd_AES, postid)
                    //println(id,postid,de_post)
                    var readid=readcounter.addAndGet(1).toString
                }
                
            }
    	}



        case "Start" =>{
        	numcomplete=counter.get()
		    if(numcomplete==nrofuser){
                friends.clear()
		    	run1.cancel()                
		    	Initialize_friend(nroffriends)
                if_verify=verify(id)
                if(if_verify==true){
                //println("Client "+id+" finishes friends Initialization")
                //println(id,"postfreq",postfreq/1000,readfreq/1000)

                system.scheduler.schedule(0 milliseconds,
                                                  postfreq milliseconds, 
                                                  self, 
                                                  Sendpost)

                system.scheduler.schedule(0 milliseconds,
                                                   readfreq milliseconds, 
                                                   self, 
                                                   Readpost(id))
                }
                if(id==1) {system.scheduler.schedule(0 seconds,
                                                   3 seconds, 
                                                   self, 
                                                   "Statics")  }             
		    	
		    }
        }
        case "Msg_not_friend"=>{
            //println("Msg_not_friend")
        }
        case "Statics"=>{
            println(postcounter.get(),postcounter.get()-post_last,readcounter.get(),readcounter.get()-read_last)
            post_last=postcounter.get()
            read_last=readcounter.get()
        }





    }

}

object Main {

  def main(args: Array[String]) {
    if(args.length < 1) {
        System.err.println("Wrong inputs!!")
        System.err.println("Usage: run <nrofuser>")
        System.exit(1)
    }
    var nrofuser=args(0).toInt
	

    implicit val system = ActorSystem("LocalSystem")

   
    for (i <- 1 to nrofuser) {
		val localActor= system.actorOf(Props(new LocalActor(system,nrofuser)),name=i+"")
		localActor ! Initialize
		//localActor ! "Start"
    }
  }

}




  
  
  
