package nosen.appengine.datastore

import com.google.appengine.api._
import datastore.Entity
import datastore.Key
import datastore.GeoPt
import datastore.IMHandle
import datastore.Query
import com.google.appengine.api.users.User
import com.google.appengine.api.blobstore.BlobKey
import java.util.Date

trait Properties {this:Kind =>

  trait Property[A] extends Member[A] {
    val name = this.getClass.getSimpleName.split("\\$").last

    def apply(w:Wrapper) = {
      val v = w.entity.getProperty(name)
      if(v == null) throw new NoSuchPropertyException(kindName, name)
      else cast(v)
    }

    def update(entity:Entity, value:A) {
      entity.setProperty(name, toRawValue(value))
    }

    def isDefinedAt(w:Wrapper) = w.entity.hasProperty(name)
    def cast(value:Any):A = value.asInstanceOf[A]
    def toRawValue(value:A):Any = value

    //Filters
    def ===(other:A):Properties.this.Filter[A] = Filter(this, other, Query.FilterOperator.EQUAL)
    def !==(other:A):Properties.this.Filter[A] = Filter(this, other, Query.FilterOperator.NOT_EQUAL)
    def <=(other:A):Properties.this.Filter[A] = Filter(this, other, Query.FilterOperator.LESS_THAN_OR_EQUAL)
    def <(other:A):Properties.this.Filter[A] = Filter(this, other, Query.FilterOperator.LESS_THAN)
    def >=(other:A):Properties.this.Filter[A] = Filter(this, other, Query.FilterOperator.GREATER_THAN_OR_EQUAL)
    def >(other:A):Properties.this.Filter[A] = Filter(this, other, Query.FilterOperator.GREATER_THAN)

    implicit def prop2sort(p:Property[A]) = Sort(this)

    def asc = Sort(this, Query.SortDirection.ASCENDING)
    def desc = Sort(this, Query.SortDirection.DESCENDING)
  }

  trait WithDefault[A] {this:Property[A] =>
    val defaultValue:A
  }

  class StringProperty extends Property[String]


  class BooleanProperty extends Property[Boolean]

  class ShortBlobProperty extends Property[Seq[Byte]] {
    import com.google.appengine.api.datastore.ShortBlob
    override def cast(value:Any):Seq[Byte] = 
      value.asInstanceOf[ShortBlob].getBytes

    override def toRawValue(value:Seq[Byte]):Any = new ShortBlob(value.toArray)
  }


  class BlobProperty extends Property[Seq[Byte]] {
    import com.google.appengine.api.datastore.Blob
    override def cast(value:Any):Seq[Byte] = 
      value.asInstanceOf[Blob].getBytes

    override def toRawValue(value:Seq[Byte]):Any = new Blob(value.toArray)
  }

  class CategoryProperty extends Property[String] {
    import com.google.appengine.api.datastore.Category
    override def cast(value:Any):String = 
      value.asInstanceOf[Category].getCategory
    override def toRawValue(value:String):Any = new Category(value)
  }

  class DateProperty extends Property[Date]

  class EmailProperty extends Property[String] {
    import com.google.appengine.api.datastore.Email
    override def cast(value:Any):String = 
      value.asInstanceOf[Email].getEmail
    override def toRawValue(value:String):Any = new Email(value)
  }

  class FloatProperty extends Property[Float] {
    override def cast(value:Any):Float = 
      value.asInstanceOf[Double].toFloat
  }

  class DoubleProperty extends Property[Double]

  class GeoPtProperty extends Property[GeoPt] 

  class UserProperty extends Property[User]

  class BlobKeyProperty extends Property[BlobKey]

  class IntProperty extends Property[Int] {
    override def cast(value:Any):Int = value.asInstanceOf[Long].toInt
  }

  class LongProperty extends Property[Long]

  class LinkProperty extends Property[String] {
    import com.google.appengine.api.datastore.Link
    override def cast(value:Any):String = 
      value.asInstanceOf[Link].getValue
    override def toRawValue(value:String):Any = new Link(value)
  }

  class IMHandleProperty extends Property[IMHandle]

  class PostalAddressProperty extends Property[String] {
    import com.google.appengine.api.datastore.PostalAddress
    override def cast(value:Any):String = 
      value.asInstanceOf[PostalAddress].getAddress
    override def toRawValue(value:String):Any = new PostalAddress(value)
  }

  class RatingProperty extends Property[Int] {
    import com.google.appengine.api.datastore.Rating
    override def cast(value:Any):Int =
      value.asInstanceOf[Rating].getRating
    override def toRawValue(value:Int):Any = new Rating(value)
  }

  class PhoneNumberProperty extends Property[String] {
    import com.google.appengine.api.datastore.PhoneNumber
    override def cast(value:Any):String = 
      value.asInstanceOf[PhoneNumber].getNumber
    override def toRawValue(value:String):Any = new PhoneNumber(value)
  }

  class TextProperty extends Property[String] {
    import com.google.appengine.api.datastore.Text
    override def cast(value:Any):String = 
      value.asInstanceOf[Text].getValue
    override def toRawValue(value:String):Any = new Text(value)
  }

  class HasA[A <:Kind#Wrapper](kind:OtherKind[A]) extends Property[A] {
    override def cast(value:Any):A = 
      kind.findByKey(value.asInstanceOf[Key]).getOrElse(kind.create)

    override def toRawValue(value:A):Any = value.key
  }

}
