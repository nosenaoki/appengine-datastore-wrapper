package nosen.appengine.datastore

import com.google.appengine.api._
import datastore.Entity
import datastore.DatastoreServiceFactory

class EntityWrapper[A <: Kind](val entity:Entity, val kind:A) { 

  def key = entity.getKey

  private def datastore = DatastoreServiceFactory.getDatastoreService

  type PropertyEntry[V] = (kind.Property[V], V)

  def bind(props:PropertyEntry[_]*) {
    props.foreach {
      case (pdef, value) => entity.setProperty(pdef.name, value)
    }
  }

  def update[V](pdef:kind.Property[V], value:V) { 
    entity.setProperty(pdef.name, value)
  }

  def apply[V](pdef:kind.Property[V]):V = {
    val v = entity.getProperty(pdef.name)
    if(v == null) throw new NoSuchPropertyException(kind.kindName, pdef.name)
    else pdef.cast(v)
  }

  def apply[K <: Kind](parent:kind.Parent[K]):parent.parentKind.Wrapper = parent.get(entity)
  
  def apply[K <: Kind](desc:kind.Descendant[K]):desc.descendantKind.QueryWrapper = desc.get(entity)

  def +[K <: Kind](desc:kind.Descendant[K]):desc.descendantKind.Wrapper = desc.create(entity)

  def get[V](pdef:kind.Property[V]):Option[V] = {
    val v = entity.getProperty(pdef.name)
    if(v == null) None
    else Some(pdef.cast(v))
  }


  def save {
    val ds = datastore
    val txn = ds.getCurrentTransaction(null)

    if(txn == null) {
      ds.put(entity)
    } else {
      ds.put(txn, entity)
    }
    this
  }

  def delete {
    val ds = datastore
    val txn = ds.getCurrentTransaction(null)

    if(txn == null) {
      ds.delete(key)
    } else {
      ds.delete(txn, key)
    }
    this

  }

}
