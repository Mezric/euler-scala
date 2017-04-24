import scala.collection.mutable.HashMap

object Euler52 {
  
  def main(args:Array[String]){
    
    def verify(x:Int): Boolean = {
      def toMap(y:Int): HashMap[Int,Int] = {
        val str = y.toString().toCharArray()
        val map = new HashMap[Int,Int]
        for(char <- str){
          if(map.contains(char)){
            map(char) = map(char)+1
          }
          else{
            map(char) = 1
          }
        }
        return map
      }
      def equalMap(map1: HashMap[Int,Int], map2:HashMap[Int,Int]):Boolean = {
        (for(x<-map1.keys)
          yield map1.get(x) == map2.get(x))
          .fold(true)(_&_) &&
        (for(x<-map2.keys)
          yield map2.get(x) == map1.get(x))
          .fold(true)(_&_)
      }
      
      val map = toMap(x)
      for(y <- 2 to 6){
        val otherMap = toMap(x*y)
        if(!equalMap(map,otherMap)) return false
      }
      return true
    }
    
    var y = 1
    while(!verify(y)){
      y += 1
    }
    
    System.out.println(y)
    
  }
}