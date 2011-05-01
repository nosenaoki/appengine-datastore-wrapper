package nosen.appengine

import com.google.appengine.api.datastore.Entity
import com.google.appengine.api.datastore.Key
import com.google.appengine.api.datastore.DatastoreService

package object datastore {
  type OtherKind[A <: Kind#Wrapper] = {
    def create:A
    def findByKey(key:Key):Option[A]
    def newInstance(entity:Entity):A
    def newInstanceAsChild(parent:Key):A
  }
}
