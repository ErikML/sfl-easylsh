package easylsh

import breeze.linalg.DenseMatrix
import breeze.stats.distributions.Gaussian
import scala.collection.parallel.immutable.{ParRange,ParSeq}
import scala.math.{acos, log, pow, ceil, sqrt, Pi}
import scala.collection.mutable

object Lsh {
  
  def halfAndHash(
      data: DenseMatrix[Double],
      numHalfAndHash: Int,
      lenHalfAndHash: Int,
      gaussian01Matrix: DenseMatrix[Double])
    : ParSeq[Array[Int]] = {
    val randomProjections = data * gaussian01Matrix
    val numPoints = data.cols
    val pr = ParRange(0,numPoints,step=1,inclusive=false)
    val hashedData: ParSeq[Array[Int]] = pr.map {i =>
      val row = randomProjections(i,::).t.toArray
        convertRow(row, lenHalfAndHash)
    }
    hashedData
  }
  
  def convertRow(row: Array[Double], lenHalfAndHash: Int): Array[Int] = {
    row.grouped(lenHalfAndHash).map {group =>
      group.foldLeft(0){(acc: Int,curr: Double) =>
        2 * acc + (if(curr >= 0) 1 else 0)
      }
    }.toArray
  }
  
  def andHash(
      data: DenseMatrix[Double],
      numHalfAndHash: Int,
      lenHalfAndHash: Int,
      gaussian01Matrix: DenseMatrix[Double])
    : Vector[Vector[(Int, Int)]] = {
    val halfHash = halfAndHash(data, numHalfAndHash, lenHalfAndHash, gaussian01Matrix)
    halfHash.map(arr => combinations(arr)).seq.toVector
  }
  
  def combinations(arr: Array[Int]): Vector[(Int, Int)] = {
    val comb = for(
      i <- 0 to (arr.length - 2);
      j <- i+1 to (arr.length - 1)
    ) yield (arr(i), arr(j))
    comb.toVector
  }
  
  def hashTable(andHashes: Vector[Vector[(Int,Int)]]): Vector[Map[(Int,Int),Set[Int]]] = {
    val numPoints = andHashes.length
    val numHashes = andHashes(0).length
    val hashIdx = ParRange(0,numHashes, step=1, inclusive=false)
    val pointIdx = Range(0,numPoints)
    hashIdx.map{j =>
      pointIdx.map{i =>
        (andHashes(i)(j),i)
      }.groupBy(_._1).map{case (k,v) => (k, v.map(_._2).toSet)}
    }.seq.toVector
  }

  def getHashParameters(
      n: Int,
      dim: Int,
      thresh: Double,
      c: Double,
      eps: Double)
    : (DenseMatrix[Double], Int, Int) = {
    val p1 = 1 - acos(thresh) / Pi
    val p2 = 1 - acos(thresh * c) / Pi
    val numAnd = log(n) / log(1 / p1)
    val rho = log(1 / p1) / log(1 / p2)
    val numOr = log(1 / eps) * pow(n,rho)
    val numHalfAndHash = ceil(0.5 * (1 + sqrt(8 * numOr + 1))).toInt
    val lenHalfAndHash = ceil(0.5 * numAnd).toInt
    val gaussian01 = Gaussian(0,1)
    val gaussian01Matrix = DenseMatrix.rand(dim, numHalfAndHash * lenHalfAndHash, gaussian01)
    (gaussian01Matrix, numHalfAndHash, lenHalfAndHash)
  }
  
  def getDataMatrix(data: Vector[Vector[Double]]): DenseMatrix[Double] = {
    DenseMatrix(data:_*)
  }
  
  def buildGraph(
      V: Vector[Vector[Double]],
      I: Vector[Vector[Double]],
      sim: (Vector[Double], Vector[Double]) => Double,
      VHashTableCol: Vector[Vector[Map[(Int, Int), Set[Int]]]],
      IHashCol: Vector[Vector[Vector[(Int,Int)]]],
      thresh: Vector[Double],
      c: Double,
      t: Int)
    : Map[Int, Set[Int]] = {
    val ItNN = ParRange(0, I.length, step=1, inclusive=false).flatMap{i =>
      val tNN = mutable.Set[Int]()
      var currBigTable = 0
      var currLittleTable = 0
      while(tNN.size < t && currBigTable < VHashTableCol.length) {
        val currHash = IHashCol(currBigTable)(i)(currLittleTable)
        val currCollisions = VHashTableCol(currBigTable)(currLittleTable).getOrElse(currHash,Set[Int]())
        val currNN = currCollisions.filter(j => sim(V(j), I(i)) >= c * thresh(currBigTable))
        tNN ++= currNN
        if(currLittleTable >= VHashTableCol(currBigTable).length - 1) {
          currLittleTable = 0
          currBigTable += 1
        } else {
          currLittleTable += 1
        }
      }
      tNN.map(j => (j,i))
    }
    ItNN.groupBy(_._1).map{case (k,v) => (k, v.map(_._2).toSet.seq)}.seq
  }
}