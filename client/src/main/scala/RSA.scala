import java.security._
import java.security.spec.X509EncodedKeySpec
import javax.crypto._
import org.apache.commons.codec.binary.Base64
import org.apache.commons.codec.binary.Hex
import javax.xml.bind.DatatypeConverter
import java.math.BigInteger;
import java.security.KeyFactory;
import java.security.Security;
import java.security.interfaces.RSAPrivateKey;
import java.security.interfaces.RSAPublicKey;
import java.security.spec.RSAPrivateKeySpec;
import java.security.spec.RSAPublicKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.util
import javax.crypto.spec.{IvParameterSpec,SecretKeySpec}
import javax.crypto.{Cipher, KeyGenerator, SecretKey}

object RSA {


  def encrypt(text:String,key:PrivateKey):String = {
    var cipher = Cipher.getInstance("RSA");
    cipher.init(Cipher.ENCRYPT_MODE, key);
    var cipherText = cipher.doFinal(text.getBytes());
    var temp = Base64.encodeBase64String(cipherText);
    temp
  }

  def encrypt(text:String,key:PublicKey):String = {
    var cipher = Cipher.getInstance("RSA");
    cipher.init(Cipher.ENCRYPT_MODE, key);
    var cipherText = cipher.doFinal(text.getBytes());
    var temp = Base64.encodeBase64String(cipherText);
    temp
  }

  def printbytes(b : Array[Byte]){
    for(x <- b){
      print(x.toInt+" ");
    }
    println("-----------------")
  }

  def decrypt(text :String,  key:PrivateKey):String  ={
    var dectyptedText = Base64.decodeBase64(text)
    var cipher = Cipher.getInstance("RSA");

    cipher.init(Cipher.DECRYPT_MODE, key);
    dectyptedText = cipher.doFinal(dectyptedText);

    new String(dectyptedText);
  }

  def decrypt(text :String,  key:PublicKey):String  ={
    var dectyptedText = Base64.decodeBase64(text)
    var cipher = Cipher.getInstance("RSA");

    cipher.init(Cipher.DECRYPT_MODE, key);
    dectyptedText = cipher.doFinal(dectyptedText);

    new String(dectyptedText);
  }



  def generateKey: KeyPair = {
    var keyGen = KeyPairGenerator.getInstance("RSA");
    keyGen.initialize(1024);
    var keypair = keyGen.genKeyPair()
    keypair
  }

  def encodePublicKey(key: PublicKey): String = {
    Base64.encodeBase64String(key.getEncoded())
  }

  def decodePublicKey(encodedKey: String): PublicKey = { 
      var publicBytes = Base64.decodeBase64(encodedKey);
    var keySpec = new X509EncodedKeySpec(publicBytes);
    var keyFactory = KeyFactory.getInstance("RSA");
    var pubKey = keyFactory.generatePublic(keySpec);
    pubKey   
    }

  def encrypt(text :String,  key:String ):String = {
    var publicKey = decodePublicKey(key)
    encrypt(text,publicKey)     
    }

  def decrypt(text :String,  key:String ):String = {
    var publicKey = decodePublicKey(key)
    decrypt(text,publicKey)     
    }
  
}


object AES {

  val ALGORITHM = "AES/CBC/PKCS5Padding"
  val CHARSET = "UTF-8"

  def encryptAES(key: SecretKey, plainText: String, ivector : String): Array[Byte] = {
    //Instantiate the cipher
    val x = ivector.getBytes("UTF-8")
    val iv:IvParameterSpec = new IvParameterSpec(util.Arrays.copyOfRange(x, 0, 16))
    val cipher = Cipher.getInstance(ALGORITHM)
    cipher.init(Cipher.ENCRYPT_MODE, key, iv)
    val encryptedTextBytes = cipher.doFinal(plainText.getBytes(CHARSET))
    encryptedTextBytes
  }

  def decryptAES(encryptedText: Array[Byte], key: SecretKey, ivector:String): String = {
    //Instantiate the cipher
    val x = ivector.getBytes("UTF-8")
    val iv:IvParameterSpec = new IvParameterSpec(util.Arrays.copyOfRange(x, 0, 16))
    val cipher = Cipher.getInstance(ALGORITHM)
    cipher.init(Cipher.DECRYPT_MODE, key, iv)

    val decryptedTextBytes = cipher.doFinal(encryptedText)
    new String(decryptedTextBytes)
  }

  def generateAESKey: SecretKey = {
    val rand = new SecureRandom();
    val keyGen = KeyGenerator.getInstance("AES")
    keyGen.init(rand)
    keyGen.generateKey()
  }

  def AEStoString (AESKey :SecretKey) :String={
      return java.util.Base64.getEncoder().encodeToString(AESKey.getEncoded())
  } 
  def StringtoAES (str : String) :SecretKey={
      var decodedKey :Array[Byte] = java.util.Base64.getDecoder().decode(str)
      var originalKey :SecretKey=new SecretKeySpec(decodedKey, 0, decodedKey.length, "AES");
      return originalKey
  }


}


