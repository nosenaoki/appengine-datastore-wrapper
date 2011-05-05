The thin wrapper for Google AppEngine Datastore, written in Scala.
=========================================================================


## Design Goal

* The goal of this library is to provide API to

  * get or set properties in type safe way.
  * define names and types of properties which a kind can have.
  * define parent/child relationship between kinds.

## Example
    import nosen.appengine.datastore._

    //define user kind
    object User extends Kind {
      //define Properties
      object firstName extends StringProperty 
      object lastName extends StringProperty
      object age extends IntProperty
 
      //define descendant relationship
      object orders extends Order.DescendantOf(User)
    }

    object Item extends Kind {
      object itemName extends StringProperty
      object unitPrice extends IntProperty
    }

    object Order extends Kind {
      //define has-a relationship
      object item extends HasA(Item)
      //define parent relationship
      object user extends User.ParentOf(Order)
    }

    //create the entity of kind User
    val u1 = {
      import User._
      create
        .bind( // bind properties
          firstName -> "Naoki",
          lastName -> "NOSE",
          age -> 34)
        .save
    }

    {
      //access properties
      import User._
      u2(firstName) 
      u2(lastName) 
      u2(age) 
    }

For more example, see the [spec](https://github.com/nosenaoki/appengine-datastore-wrapper/blob/master/src/test/scala/nosen/appengine/datastore/KindSpec.scala)

