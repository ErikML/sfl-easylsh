import scala.collection.mutable
import scala.io.Source
import sfl.Sfl
import easylsh.Lsh
import scala.math.{sqrt, max, log, ceil}
import scala.util.Random

object Experiments extends App {
  
  def main() = {
    val (vecs, ids) = loadCsv("/movie-vecs.csv")
    val k = 2000
    val greedyEps = 0.1
    val thresh = 0.8
    val c = 0.5
    val lshEps = 0.1
    Random.setSeed(23)
    println("Graph Greedy")
    val tgs = System.nanoTime
    val AGraph = graphGreedy(vecs, k, greedyEps, thresh, c, lshEps)
    println((System.nanoTime - tgs) * 1e-9)
    println(funcVal(AGraph, vecs))
    println("StochasticGreedy")
    Random.setSeed(23)
    val tss = System.nanoTime
    val ASto = Sfl.stochasticGreedy(vecs, k, greedyEps)
    println((System.nanoTime - tss) * 1e-9)
    println(funcVal(ASto, vecs))
  }
  
  def funcVal(A: Set[Int], V: Vector[Vector[Double]]) = {
    V.map{v =>
      A.map{a => Sfl.dot(v, V(a))}.reduce(max(_,_))
    }.sum
  }
  
  def graphGreedy(vecs: Vector[Vector[Double]], k: Int, greedyEps: Double, thresh: Double, c: Double, lshEps: Double) = {
    val I = vecs.map{v => 
      val norm = sqrt(Sfl.dot(v,v))
      v.map{vi => vi / norm}
    }
    val maxNorm = vecs.map{v => sqrt(Sfl.dot(v,v))}.reduce(max(_,_))
    val V = vecs.map{v =>
      v.map{vi => vi / maxNorm}
    }
    val nV = V.length
    val dimV = V(0).length
    val (dm, nah, lah) = Lsh.getHashParameters(nV, dimV, thresh, c, lshEps)
    val dataV = Lsh.getDataMatrix(V)
    val ahV = Lsh.andHash(dataV, nah, lah, dm)
    val htV = Lsh.hashTable(ahV)
    val dataI = Lsh.getDataMatrix(I)
    val ahI = Lsh.andHash(dataI, nah, lah, dm)
    val t = ceil(nV * (1 + log(nV)) * (1.0 / k)).toInt
    val graph = Lsh.buildGraph(V, I, Sfl.dot, Vector(htV), Vector(ahI), Vector(thresh), c, t)
    println("graph made")
    val A = Sfl.stochasticGreedy(vecs, k, greedyEps, graph, true)
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
  
  main()
}