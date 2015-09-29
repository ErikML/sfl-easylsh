import scala.collection.mutable
import scala.io.Source
import sfl.Sfl
import easylsh.Lsh
import sampler.Sampler
import scala.math.{sqrt, max, log, ceil, min, floor}
import scala.util.Random
import scala.annotation.tailrec

object Experiments extends App {
  
  def main() = {
    val (vecs, ids) = loadCsv("/movie-vecs-2.csv")
    println(vecs.length)
    val k = 400
    val greedyEps = 0.1
    val thresh = 0.65
    val c = 0.5
    val lshEps = 0.1
    val shrink = 1
    println("Graph Greedy")
    val tgs = System.nanoTime
    val AGraph = graphGreedy(vecs, k, greedyEps, thresh, c, lshEps, shrink)
    val tge = System.nanoTime
    println((tge - tgs) * 1e-9)
    println(funcVal(AGraph, vecs))
    val ARand = Sampler(vecs.length).subset(k)
    System.gc()
    println("StochasticGreedy")
    val tss = System.nanoTime
    val ASto = Sfl.stochasticGreedy(vecs, k, greedyEps)
    val tse = System.nanoTime
    println((tse - tss) * 1e-9)
    println(funcVal(ASto, vecs))
    println("Random")
    println(funcVal(ARand, vecs))
    println("********************")
    println("********************")
    println("********************")
    println("StochasticGreedy")
    println(s"t: ${(tse - tss) * 1e-9}")
    println(s"v: ${funcVal(ASto, vecs)}")
    println("Graph Greedy")
    println(s"t: ${(tge - tgs) * 1e-9}")
    println(s"v: ${funcVal(AGraph, vecs)}")
    println("Random")
    println(s"v: ${funcVal(ARand, vecs)}")
    
  }
  
  def funcVal(A: Set[Int], V: Vector[Vector[Double]]) = {
    V.map{v =>
      A.map{a => Sfl.dot(v, V(a))}.reduce(max(_,_))
    }.sum
  }
  
  def graphGreedy(vecs: Vector[Vector[Double]], k: Int, greedyEps: Double, thresh: Double, c: Double, lshEps: Double, shrink: Int) = {
    val n = vecs.length
    val dim = vecs(0).length+1
    val maxNorm = vecs.map{v => sqrt(Sfl.dot(v,v))}.reduce(max(_,_))
    val V = vecs.map{v => 
      val maxNormalized = v.map(vi => vi/maxNorm)
      val newNorm = sqrt(Sfl.dot(maxNormalized,maxNormalized))
      maxNormalized :+ (1.0 - newNorm)
    }
    val (dm, nah, lah) = Lsh.getHashParameters(n, dim, thresh, c, lshEps)
    println(s"num or hashes: ${2*nah}")
    println(s"num and hashes: ${2*lah}")
    val dataV = Lsh.getDataMatrix(V)
    val ahV = Lsh.andHash(dataV, nah, lah, dm)
    val htV = Lsh.hashTable(ahV)
    val I = vecs.map{v =>
      val norm = sqrt(Sfl.dot(v,v))
      v.map(vi => vi / norm) :+ 0.0
    }
    val dataI = Lsh.getDataMatrix(I)
    val ahI = Lsh.andHash(dataI, nah, lah, dm)
    val tNaive = min(ceil(n * (1 + log(n)) * (1.0 / k)).toInt, n)/shrink
    val t = getT(n,k,1,n)/shrink
    println(s"t: $t , tNaive: $tNaive")
    val graph = Lsh.buildGraph(V, I, Sfl.dot, Vector(htV), Vector(ahI), Vector(thresh), c, t)
    println("graph made")
    val A = Sfl.stochasticGreedy(V, k, greedyEps, graph, true)
    A
  }
  
  def loadCsv(csvFilename: String): (Vector[Vector[Double]], Vector[Int])  = {
    val bs = Source.fromURL(getClass.getResource(csvFilename))
    val dataPoints = mutable.ArrayBuffer[Vector[Double]]()
    val ids = mutable.ArrayBuffer[Int]()
    for(line <- bs.getLines()) {
      val splitLine = line.split(",").map(_.trim)
      val id = splitLine.head.toInt
      val data = splitLine.tail.map(_.toDouble).toVector
      dataPoints += data
      ids += id
    }
    (dataPoints.toVector, ids.toVector)
  }
  
  @tailrec def getT(n: Int, k: Int, tMin: Int, tMax: Int): Int = {
    val currReq = (t:Int) => floor(n * (1 + log(t)) * (1.0 / t)).toInt
    val currMin = currReq(tMin)
    val currMax = currReq(tMax)
    val tMid = (tMax + tMin)/2
    val currMid = currReq(tMid)
    println(s"($tMin, $tMid, $tMax) ($currMin, $currMid, $currMax)")
    if(currMid==k||currMid==(k-1)) {
      tMid
    } else if (currMid < k-1) {
      getT(n, k, tMin, tMid)
    } else {
      getT(n,k,tMid,tMax)
    }
  }
  
  main()
}