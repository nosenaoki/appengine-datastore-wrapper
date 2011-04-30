package nosen.appengine.datastore

import org.specs._
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig;
import com.google.appengine.tools.development.testing.LocalServiceTestHelper;

class KindSpec extends Specification { 
  val helper =
    new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig());

  object User extends Kind {
    type User = Wrapper
    object firstName extends StringProperty
    object lastName extends StringProperty
    object age extends IntProperty

    object orders extends Order.DescendantOf[User]
  }

  object Order extends Kind {
    type Order = Wrapper
    object itemName extends StringProperty
    object user extends User.AncestorOf[Order]
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
      User.findAll.map(_.delete)
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

      findAll.where(age === 34).size must beEqualTo(1)
      findAll.where(age === 34).head(firstName) must beEqualTo("Naoki")

      val res = findAll where (age > 33) orderBy (age desc, firstName)
      res.size must beEqualTo(2)
      res.head(age) must beEqualTo(35)

    }

    "define relation ship between child and parent " in {
      User.findAll.foreach(_.delete)
      Order.findAll.foreach(_.delete)

      val u1 = {
	import User._
	create.bind (
          firstName -> "Naoki",
          lastName -> "NOSE",
          age -> 34)
      }

      u1.save

      val o1 = {
	import Order._
	import User.orders
	u1(orders).create
	  .bind(itemName -> "Programming in scala")
      }
      o1.save

      import User._
      u1(orders).findAll.size must beEqualTo(1)

      import Order._
      o1(user).get.key must beEqualTo(u1.key)

    }

    doLast {
      helper.tearDown
    }

  }

}

