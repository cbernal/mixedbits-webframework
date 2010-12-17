package net.mixedbits.tools

class VersionUpgrade[T](obj:T,currentVersion: => Int,updateVersion: Int => Any){
  def this(obj:T,versionProperty:DataObject#Property[Int]) = this(obj,versionProperty(),versionProperty(_))
  
  val upgrades = new ListBuffer[(Int,()=>Any)]
  def upgradeTo(version:Int)(block: => Any) = upgrades += (version,()=>block)
  
  def apply():T = {
    for( (version,block) <- upgrades if version > currentVersion )
      block()
    
    updateVersion(upgrades map {_._1} max)
    
    obj
  }
}

object VersionUpgrade{
  class VersionUpgradeTest extends DataObject{
  
    object a extends DefaultProperty("old")
    object b extends DefaultProperty("old")
    object c extends DefaultProperty("old")
    
    protected object version extends DefaultProperty(0)
    val upgrade = new VersionUpgrade(this,version){
      upgradeTo(1){ a("awesome") }
      upgradeTo(2){ b("awesome") }
      upgradeTo(3){ c("awesome") }
    }
  }
  def test(){
    println(new VersionUpgradeTest().upgrade())
  }
}
