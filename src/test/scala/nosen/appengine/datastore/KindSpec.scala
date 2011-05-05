package nosen.appengine.datastore

import org.specs._
import com.google.appengine.tools.development.testing.LocalDatastoreServiceTestConfig;
import com.google.appengine.tools.development.testing.LocalServiceTestHelper;

class KindSpec extends Specification { 
  val helper =
    new LocalServiceTestHelper(new LocalDatastoreServiceTestConfig());

  object User extends Kind {
    object firstName extends StringProperty
    object lastName extends StringProperty
    object age extends IntProperty

    object orders extends Order.DescendantOf(User)
  }

  object Item extends Kind {
    object itemName extends StringProperty
    object unitPrice extends IntProperty
  }

  object Order extends Kind {
    object itemName extends StringProperty
    object item extends HasA(Item)
    object user extends User.ParentOf(Order)
  }

  "A kind" can {
    doFirst {
      helper.setUp
    }

    "create ,save ,get, remove an entity" in {
      val u1 = {
        import User._
        create
          .bind(
            firstName -> "Naoki",
            lastName -> "NOSE",
            age -> 34)
          .save
      }

      val u2 = User.findById(u1.key.getId).get

      {
        import User._
        u2(firstName) must beEqualTo("Naoki")
        u2(lastName) must beEqualTo("NOSE")
        u2(age) must beEqualTo(34)
      }
    }

    "filter,sort resulting collection" in {
      User.findAll.map(_.delete)

      val u1 = {
        import User._
        create
          .bind(
            firstName -> "Naoki",
            lastName -> "NOSE",
            age -> 34)
          .save
      }

      val u2 = {
        import User._
        create
          .bind(
            firstName -> "Naoki",
            lastName -> "NOSE",
            age -> 35)
          .save
      }

      {
        import User._
        findAll.where(age === 34).size must beEqualTo(1)
        findAll.where(age === 34).head(firstName) must beEqualTo("Naoki")

        val res = findAll where (age > 33) orderBy (age desc, firstName)
        res.size must beEqualTo(2)
        res.head(age) must beEqualTo(35)
      }

    }

    "define relationship between child and parent " in {
      User.findAll.foreach(_.delete)
      Order.findAll.foreach(_.delete)

      val u1 = {
        import User._
        create.bind (
          firstName -> "Naoki",
          lastName -> "NOSE",
          age -> 34)
        .save
      }

      val o1:Order.Wrapper = {
        import Order._
        import User.orders
        u1(orders).create
          .bind(itemName -> "Programming in scala")
          .save
      }

      import User._
      u1(orders).findAll.size must beEqualTo(1)

      import Order._
      o1(user).get.key must beEqualTo(u1.key)

    }

    "define HasA relation ship" in {
      User.findAll.foreach(_.delete)
      Order.findAll.foreach(_.delete)
      Item.findAll.foreach(_.delete)

      val u1 = {
        import User._
        create.bind (
          firstName -> "Naoki",
          lastName -> "NOSE",
          age -> 34).save
      }
 
      val i1 = {
        import Item._
        create.bind(
          itemName -> "Proramming in Scala",
          unitPrice -> 2500).save
      }

      val o1:Order.Wrapper = {
        import User.orders
        import Order._
        u1(orders).create.bind(item -> i1).save
      }

      val o2 = Order.findByKey(o1.key).get

      val i2 = o2(Order.item)
      i2.key must beEqualTo(i1.key)
      i2(Item.unitPrice) must beEqualTo(2500)
      
    }

    doLast {
      helper.tearDown
    }

  }

}

