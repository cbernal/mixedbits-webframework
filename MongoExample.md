
```

import java.util.Date
import com.mongodb._
import net.mixedbits.mongo._
import net.mixedbits.tools.Objects._

object MongoDatabaseConnection extends MongoDatabase("somedatabasename"){
  val getConnection = new Mongo("localhost")
}

object BlogEntries extends MongoCollection(MongoDatabaseConnection,"blog")

object People extends MongoCollection(MongoDatabaseConnection,"people"){
  index(
    Person.Location.City,
    Person.Location.District,
    Person.Location.PostalCode,
    Person.Location.Latitude,
    Person.Location.Longitude
  )
}

object BlogEntry{
  object Title extends JsStringProperty
  object Text extends JsStringProperty
  object Url extends JsStringProperty
  object Categories extends JsArrayProperty[String]
}

object Person{
  object Name extends JsStringProperty
  object Location{
    object Address extends JsStringProperty
    object City extends JsStringProperty
    object State extends JsStringProperty
    object PostalCode extends JsStringProperty
    object Latitude extends JsDoubleProperty
    object Longitude extends JsDoubleProperty
  }
}

```