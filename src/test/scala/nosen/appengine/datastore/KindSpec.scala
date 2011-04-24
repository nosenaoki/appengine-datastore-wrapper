package nosen.appengine.datastore

import org.specs._

import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig;
import com.google.appengine.tools.development.testing.LocalServiceTestHelper;

class KindSpec extends Specification { 
  val helper =
    new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig());
object User extends Kind { type User = Wrapper
    val parent = None
    object firstName extends StringProperty
    object lastName extends StringProperty
    object age extends IntProperty
    
  }

  "A kind" can {
    doFirst {
      helper.setUp
    }


    "create ,save ,get, remove an entity" in {
      val u1 = User.create

      import User._

      u1.bind(
        firstName -> "Naoki",
        lastName -> "NOSE",
        age -> 34)

      u1.save

      val id = u1.key.getId
      val u2 = User.findByKey(id).get

      u2(firstName) must beEqualTo("Naoki")
      u2(lastName) must beEqualTo("NOSE")
      u2(age) must beEqualTo(34)

    }

    "filter,sort resulting collection" in {

      User.select.map(_.delete)

      val u1 = User.create

      import User._

      u1.bind(
        firstName -> "Naoki",
        lastName -> "NOSE",
        age -> 34)
     
      u1.save

      val u2 = User.create

      u2.bind(
        firstName -> "Naoki",
        lastName -> "NOSE",
        age -> 35)
     
      u2.save


      select.where(age === 34).size must beEqualTo(1)
      select.where(age === 34).head(firstName) must beEqualTo("Naoki")
      

      val res = select where (age > 33) orderBy (age desc, firstName)
      res.size must beEqualTo(2)
      res.head(age) must beEqualTo(35)

    }
    doLast {
      helper.tearDown
    }

  }


}
