package nosen.appengine.datastore

class NoSuchPropertyException(kind:String, propertyName:String) 
  extends RuntimeException("kind=" + kind + ", property=" + propertyName)
