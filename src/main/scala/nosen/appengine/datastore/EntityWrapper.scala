package nosen.appengine.datastore

import com.google.appengine.api._
import datastore.Entity
import datastore.DatastoreServiceFactory

class EntityWrapper[A <: Kind](val entity:Entity, val kind:A) { 

  def key = entity.getKey

  private def datastore = DatastoreServiceFactory.getDatastoreService

  type PropertyEntry[V] = (kind.Property[V], V)

  def bind(props:PropertyEntry[_]*):this.type = {
    props.foreach {
      case (pdef, value) => entity.setProperty(pdef.name, value)
    }
    this
  }

  def update[V](pdef:kind.Property[V], value:V) { 
    entity.setProperty(pdef.name, value)
  }

  def apply[V](op:PartialFunction[this.type, V]):V = op(this)

  def get[V](op:PartialFunction[this.type, V]):Option[V] = op.lift(this)

  def save = {
    val ds = datastore
    val txn = ds.getCurrentTransaction(null)

    if(txn == null) {
      ds.put(entity)
    } else {
      ds.put(txn, entity)
    }
    this
  }

  def delete = {
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
