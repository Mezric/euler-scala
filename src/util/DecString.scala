package util

import UsefulLambdas.countTrue

// Stores a string of decimals for problem-solving

class DecString(val nums:Array[Int]) {
  
  def apply(index:Int) = nums(index)
  def length = nums.length
  
  def map[T](f:Int=>T) = nums.map(f)
  def map(f:Int=>Int) = new DecString(nums.map(f))
  
  def zipWithIndex = nums.zipWithIndex
  
  def matches(o:DecString):Int = {
    nums.zip(o.nums)
      .map((pair:(Int,Int)) => pair._1==pair._2)
      .map(countTrue)
      .sum
  }
  
  def this(str:String){
    this(str.map((c:Char) => Integer.parseInt("" + c)).toArray)
  }
}