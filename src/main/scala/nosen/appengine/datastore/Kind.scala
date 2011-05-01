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

  type OtherKind[A <: Kind#Wrapper] = {
    def newInstance(entity:Entity):A
  }

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

    override def cast(value:Any):A = {
      val key = value.asInstanceOf[Key]
      val entity = datastore.get(key)
      if(entity == null)
	kind.newInstance(new Entity(value.asInstanceOf[Key]))
      else 
	kind.newInstance(entity)
    }

    override def toRawValue(value:A):Any = value.key
  }

  case class KeyWrapper(key:Key) 

  object KeyWrapper {
    implicit def wrapId(id:Long):Kind.this.KeyWrapper = 
      new KeyWrapper(KeyFactory.createKey(kindName, id))

    implicit def wrapName(name:String):Kind.this.KeyWrapper = 
      new KeyWrapper(KeyFactory.createKey(kindName, name))
    implicit def wrapKey(key:Key):Kind.this.KeyWrapper = 
      new KeyWrapper(key)
  }

  case class QueryWrapper(ancestor:Option[Key] = None,
      filter:Seq[Filter[_]] = Seq.empty, sort:Seq[Sort] = Seq.empty,
      limitOpt:Option[Int] = None, offsetOpt:Option[Int] = None) extends Iterable[Wrapper] {

    def iterator:Iterator[Wrapper] = {
      val ds = datastore 
      ds.prepare(query).asIterator(fetchOptions).map(newInstance)
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

  case class Descendant[A <:Kind#Wrapper](wrapper:A) {
    def findAll = childrenOf(wrapper.key)
    def create = newInstanceAsChild(wrapper.key)
  }

  trait DescendantOf[A <: Kind#Wrapper] extends PartialFunction[A, Descendant[A]] {
    def apply(wrapper:A) = Descendant(wrapper)
    def isDefinedAt(wrapper:A) = true
  }

  trait AncestorOf[A <: Kind#Wrapper] extends PartialFunction[A, Option[Wrapper]] {
    def apply(descendant:A) = findByKey(descendant.key.getParent)
    def isDefinedAt(descendant:A) = true
  }

  def findByKey(key:KeyWrapper):Option[Wrapper] = 
    try {
      Some(newInstance(datastore.get(key.key)))
    } catch {
      case e:EntityNotFoundException => None
    }

  val findAll = QueryWrapper()

  def childrenOf(ancestor:Key) = QueryWrapper(Some(ancestor))

  def create:Wrapper = newInstance(new Entity(kindName))

  protected def newInstance(keyname:String):Wrapper = 
    newInstance(new Entity(kindName, keyname))
  def newInstanceAsChild(parent:Key):Wrapper = 
    newInstance(new Entity(kindName, parent))
  def newInstance(entity:Entity):Wrapper = new Wrapper(entity, this)

}

