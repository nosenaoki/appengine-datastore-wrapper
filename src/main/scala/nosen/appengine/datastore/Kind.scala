package nosen.appengine.datastore

import collection.mutable.Map
import com.google.appengine.api._
import datastore.Entity
import datastore.Key
import datastore.KeyFactory
import datastore.EntityNotFoundException
import datastore.DatastoreServiceFactory
import datastore.Query
import datastore.FetchOptions
import collection.JavaConversions._

trait Kind {
  val kindName :String = this.getClass.getSimpleName.split("\\$").last

  def datastore = DatastoreServiceFactory.getDatastoreService

  type Wrapper = EntityWrapper[this.type]
  type Member[A] = PartialFunction[Wrapper, A]

  case class Filter[A](prop:Kind.this.Property[A], value:A ,op:Query.FilterOperator) {
    def rawValue:Any = prop.toRawValue(value)
  }

  case class Sort(prop:Kind.this.Property[_], direction:Query.SortDirection = Query.SortDirection.ASCENDING)

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
    def ===(other:A):Kind.this.Filter[A] = Filter(this, other, Query.FilterOperator.EQUAL)
    def !==(other:A):Kind.this.Filter[A] = Filter(this, other, Query.FilterOperator.NOT_EQUAL)
    def <=(other:A):Kind.this.Filter[A] = Filter(this, other, Query.FilterOperator.LESS_THAN_OR_EQUAL)
    def <(other:A):Kind.this.Filter[A] = Filter(this, other, Query.FilterOperator.LESS_THAN)
    def >=(other:A):Kind.this.Filter[A] = Filter(this, other, Query.FilterOperator.GREATER_THAN_OR_EQUAL)
    def >(other:A):Kind.this.Filter[A] = Filter(this, other, Query.FilterOperator.GREATER_THAN)

    implicit def prop2sort(p:Property[A]) = Sort(this)

    def asc = Sort(this, Query.SortDirection.ASCENDING)
    def desc = Sort(this, Query.SortDirection.DESCENDING)
  }

  trait WithDefault[A] {this:Property[A] =>
    val defaultValue:A
  }

  class StringProperty extends Property[String]

  class IntProperty extends Property[Int] {
    override def cast(value:Any):Int = value.asInstanceOf[Long].toInt
  }

  class HasA[A <:Kind#Wrapper](kind:OtherKind[A]) extends Property[A] {
    override def cast(value:Any):A = 
      kind.findByKey(value.asInstanceOf[Key]).getOrElse(kind.create)

    override def toRawValue(value:A):Any = value.key
  }

  class Descendant[A <:Kind#Wrapper](wrapper:A) {
    def findAll:QueryWrapper[Wrapper] = childrenOf(wrapper.key)
    def create:Wrapper = newInstanceAsChild(wrapper.key)
  }

  class DescendantOf[A <: Kind#Wrapper](kind:OtherKind[A]) extends PartialFunction[A, Descendant[A]] {
    def apply(wrapper:A) = new Descendant(wrapper)
    def isDefinedAt(wrapper:A) = true
  }

  class ChildOf[A <: Kind#Wrapper](kind:OtherKind[A]) extends PartialFunction[Wrapper, Option[A]] {
    def apply(descendant:Wrapper) = kind.findByKey(descendant.key.getParent)
    def isDefinedAt(descendant:Wrapper) = true
  }

  case class QueryWrapper[A <: Kind#Wrapper](
    kind:OtherKind[A], ancestor:Option[Key] = None,
    filter:Seq[Filter[_]] = Seq.empty, sort:Seq[Sort] = Seq.empty,
    limitOpt:Option[Int] = None, offsetOpt:Option[Int] = None) extends Iterable[A] {

    def iterator:Iterator[A] = {
      val ds = datastore 
      ds.prepare(query).asIterator(fetchOptions).map(kind.newInstance)
    }

    def fetchOptions = {
      import FetchOptions.Builder._
      
      val fo = withDefaults
      limitOpt.foreach(fo.limit)
      offsetOpt.foreach(fo.offset)
      fo
    } 
    
    def query = {
      val q = ancestor.map(new Query(kindName, _))getOrElse(new Query(kindName))
      for(f <- filter) q.addFilter(f.prop.name, f.op, f.rawValue) 
      for(s <- sort) q.addSort(s.prop.name, s.direction)
      q
    }

    def where(f:Kind.this.Filter[_]*) = copy(filter = (filter ++ f))
    def orderBy(s:Kind.this.Sort*) = copy(sort = (sort ++ s)) 
    def limit(i:Int) = copy(limitOpt = Some(i))
    def offset(i:Int) = copy(offsetOpt = Some(i))
  }

  def findById(id:Long):Option[Wrapper] = findByKey(KeyFactory.createKey(kindName, id))
  def findByName(name:String):Option[Wrapper] = findByKey(KeyFactory.createKey(kindName, name))
  def findByKey(key:Key):Option[Wrapper] = {
    if(key.getKind != kindName) 
      throw new IllegalArgumentException("A kind of key must be same as " + 
					 kindName + "but is " + key.getKind) 
    try {
      Some(newInstance(datastore.get(key)))
    } catch {
      case e:EntityNotFoundException => None
    }
  }

  val findAll:QueryWrapper[Wrapper] = QueryWrapper(kind = this)

  def childrenOf(ancestor:Key):QueryWrapper[Wrapper] = QueryWrapper(kind= this, ancestor = Some(ancestor))

  def create:Wrapper = newInstance(new Entity(kindName))

  def newInstance(keyname:String):Wrapper = 
    newInstance(new Entity(kindName, keyname))
  def newInstanceAsChild(parent:Key):Wrapper = 
    newInstance(new Entity(kindName, parent))
  def newInstance(entity:Entity):Wrapper = new Wrapper(entity, this)

}

