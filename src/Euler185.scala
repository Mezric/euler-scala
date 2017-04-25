// Zachary Sullivan
// Project Euler - Problem 185
// 'Number Mind'

// in progress

import scala.collection.mutable.HashMap
import scala.io.Source

import util.DecString

object Euler185 {
  
  class Guess(val nums:DecString, val correct:Int)
  
  def solve(guesses:Array[Guess]): Option[DecString] = {
    
    val indexed_guesses = guesses.zip(0 until guesses.length)
    
    // Returns Some(True|False) if the answer is certain
    // Returns None otherwise
    def test_solution(solution:DecString): Option[Boolean] = {
      val missing = solution.nums
        .map((x:Int) => if(x == -1) 1 else 0)
        .fold(0)(_+_)
      
      val ret = guesses.map((g:Guess) => (g.nums.matches(solution), g.correct))
        .map((x:(Int,Int)) => 
          if(x._1==x._2) 1
          else if(x._1 > x._2) -1
          else if(x._1 >= x._2-missing) 0
          else -1
        )
        .fold(1)((a:Int,b:Int) =>
          (a, b) match{
            case (1,1) => 1
            case (0,1) => 0
            case (1,0) => 0
            case _ => -1
          }
         )
         
      (ret, missing) match{
        case (1,0) => Some(true)
        case (_,0) => Some(false)
        case (0,_) => None
        case (-1,_) => Some(false)
      }
    }
    
    def rec(csets:Map[Int,List[Guess]], possibles:HashMap[(Int,Int), Boolean], progress:DecString): Option[DecString] = {
      
      val goal = test_solution(progress)
      if(goal.exists((b:Boolean) => true)){
        return if(goal.get) Some(progress) else None
      }
      
      val minCorrect = csets.keySet.min
      
      val guess = csets(minCorrect).head
      val remainder = new HashMap[Int,List[Guess]]
      remainder ++: csets
      remainder(minCorrect) = remainder(minCorrect).tail
      
      val unassigned = progress.zipWithIndex.filter((x:(Int,Int)) => x._1 == -1)
      
      if(minCorrect == 0){
        val sub_poss = possibles.clone()
        for(x <- 0 until guess.nums.length){
          //if(guess(x) != progress(x))
        }
      }
      
      None
    }
    
    val correct_sets = new HashMap[Int, List[Guess]]
    for(i <- 0 until 16)
      correct_sets(i) = List.empty[Guess]
    for(g <- guesses){
      correct_sets(i) ::= g
    }
    val progress = new DecString((for(i <- 0 until 16) yield -1).toArray)
    val possibles = new HashMap[(Int,Int), Boolean]
    for(i <- 0 until 16; j <- 0 until 10){
      possibles(i->j) = true
    }
    rec(correct_sets.toMap, possibles, progress)
  }
  
  def main(args:Array[String]){
    val stream = Source.fromFile("Euler185.txt")
    val reader = stream.bufferedReader()
    
    def f(s:String):Guess = {
       val arr = s.split(";")
       new Guess(new DecString(arr(0)), Integer.parseInt(arr(1)))
    }
    
    val guesses = (
      for(n <- 0 until 22) yield f(stream.bufferedReader().readLine())
    ).toArray
    
    solve(guesses)
  }
}