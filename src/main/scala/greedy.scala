package sfl

import scala.util.Random
import scala.collection.mutable
import scala.collection.parallel.immutable.{ParRange, ParSet}
import scala.math.{log, ceil, sqrt}
import sampler.Sampler

object Sfl {
  
  def stochasticGreedy(V: Vector[Vector[Double]], k: Int, eps: Double): Set[Int] = {
    val n = V.length
    val sampler = Sampler(n)
    stochasticGreedy(V, k, eps, sampler)
  }
  
  def stochasticGreedy(V: Vector[Vector[Double]], k: Int, eps: Double, graph: Vector[ParSet[(Int, Double)]], rescale: Boolean): Set[Int] = {
    val n = V.length
    val sampler = Sampler(n)
    stochasticGreedy(V, k, eps, graph, rescale, sampler)
  }
  
  def stochasticGreedy(V: Vector[Vector[Double]], k: Int, eps: Double, sampler: Sampler): Set[Int] = {
    val n = V.length
    val currMaxNeighbor = Array.fill[Double](n)(0.0)
    val prevGain = Array.fill[Double](n)(Double.PositiveInfinity)
    var currIter = 0
    val randomSetSize = ceil(n * (1.0 / k) * log(1 / eps)).toInt
    val r = ParRange(0, n, step=1, inclusive=false)
    val getGain = {i: Int =>
      r.map {j =>
        val cij = dot(V(i), V(j))
        val curr = currMaxNeighbor(j)
        val jGain = if(cij > curr) cij - curr else 0
        jGain
      }.sum
    }
    val A = mutable.Set[Int]()
    while(currIter < k) {
      val randomSet = sampler.subset(randomSetSize)
      val sorted = randomSet.par.toVector.sortWith(prevGain(_) < prevGain(_))
      var currMaxElem = sorted(0)
      var currMaxGain = getGain(sorted(0))
      for(i <- 1 to sorted.length-1) {
        if(prevGain(sorted(i)) > currMaxGain) {
          val currGain = getGain(sorted(i))
          if(currGain > currMaxGain) {
            currMaxElem = sorted(i)
            currMaxGain = currGain
          }
        }
      }
      r.foreach{j =>
        val cij = dot(V(currMaxElem), V(j))
        val curr = currMaxNeighbor(j)
        if(cij > curr) {
          currMaxNeighbor(j) = cij
        }
      }
      sampler.removePrevSeenElem(currMaxElem)
      A.add(currMaxElem)
      currIter += 1
    }
    A.toSet
  }
  
  def stochasticGreedy(V: Vector[Vector[Double]], k: Int, eps: Double, graph: Vector[ParSet[(Int, Double)]], rescale: Boolean, sampler: Sampler): Set[Int] = {
    val n = V.length
    val currMaxNeighbor = Array.fill[Double](n)(0.0)
    val prevGain = Array.fill[Double](n)(Double.PositiveInfinity)
    var currIter = 0
    val randomSetSize = ceil(n * (1.0 / k) * log(1 / eps)).toInt
    val getGain = {i: Int =>
      graph(i).map {case (j, cij) =>
        val scaleFactor = if(rescale) sqrt(dot(V(i), V(i))) else 1
        val curr = currMaxNeighbor(j)
        val jGain = if(cij > curr) cij - curr else 0
        scaleFactor * jGain
      }.sum
    }
    val A = mutable.Set[Int]()
    while(currIter < k) {
      val randomSet = sampler.subset(randomSetSize)
      val sorted = randomSet.par.toVector.sortWith(prevGain(_) < prevGain(_))
      var currMaxElem = sorted(0)
      var currMaxGain = getGain(sorted(0))
      for(i <- 1 to sorted.length-1) {
        if(prevGain(sorted(i)) > currMaxGain) {
          val currGain = getGain(sorted(i))
          if(currGain > currMaxGain) {
            currMaxElem = sorted(i)
            currMaxGain = currGain
          }
        }
      }
      graph(currMaxElem).foreach{case(j,cij) =>
        val scaleFactor = if(rescale) sqrt(dot(V(currMaxElem), V(currMaxElem))) else 1.0
        val curr = currMaxNeighbor(j)
        if(cij * scaleFactor > curr) {
          currMaxNeighbor(j) = cij * scaleFactor
        }
      }
      sampler.removePrevSeenElem(currMaxElem)
      A.add(currMaxElem)
      currIter += 1
    }
    A.toSet
  }
  
  def dot(x: Vector[Double], y: Vector[Double]) = {
    var total = 0.0
    var i = 0
    val d = x.length
    while(i < d) {
      total += x(i) * y(i)
      i += 1
    }
    total
  }
}