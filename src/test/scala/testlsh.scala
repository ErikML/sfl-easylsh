package easylsh.test

import easylsh.Lsh
import org.scalatest.{FlatSpec, Matchers}
import breeze.linalg.DenseMatrix

class LshTest extends FlatSpec with Matchers{
  behavior of "Lsh Example"
  
  
  
  it should "correctly convert a random projection to integer hashes" in {
    val row1 = Array(0.3, 0.1, -1.1, 0.4, -0.4, -0.9, -1.4, 0.01, 0.03)
    val lenHalfAndHash1 = 3
    // Should convert row to [b110, b100, b011] = [6, 4, 3]
    Lsh.convertRow(row1, lenHalfAndHash1) should === (Array(6,4,3))
    val row2 = Array(0.9, 1.001, -0.003, 12.0, 4.5, -0.6, -11.0, -3.0)
    val lenHalfAndHash2 = 2
    // Should convert row to [b11, b01, b10, b00] = [3, 1, 2, 0]
    Lsh.convertRow(row2, lenHalfAndHash2) should === (Array(3, 1, 2, 0))
  }
  
  val data = DenseMatrix(
    (13.7104110, 4.52424159, 11.5473712, 15.8554059),
    (22.7498826, 22.7709215, 19.6094131, 4.09709252),
    (0.0167698975, 16.6642695, 22.8387298, 20.8250903),
    (22.2231955, 13.2941450, 21.5702394, 21.6602337))
  val gaussian01Matrix = DenseMatrix(
    (0.43286322, 0.63966468, 1.1510637, -0.99341959),
    (-1.22562975, 1.64461651, 0.4697416, -2.19903841),
    (1.73195824, -0.77826703, -0.47204024, 1.49938599),
    (-0.50904327, -2.05844279, 0.41810137, 0.71138387))
  
  
  it should "correctly generate the halfAndHashes" in {
    val lenHalfAndHash1 = 2
    val numHalfAndHash1 = 2
    val halfAndHash = Lsh.halfAndHash(data, numHalfAndHash1, lenHalfAndHash1, gaussian01Matrix)
    halfAndHash.toArray should === (Array(Array(2,3), Array(3,2), Array(2,3), Array(2,2)))
  }
  
  it should "correctly generate the full hashes and hash tables" in {
    val lenHalfAndHash2 = 1
    val numHalfAndHash2 = 4
    val fullHash = Lsh.andHash(data, numHalfAndHash2, lenHalfAndHash2, gaussian01Matrix)
    val hash1 = Vector((1,0), (1,1), (1,1), (0,1), (0,1), (1,1))
    val hash2 = Vector((1,1), (1,1), (1,0), (1,1), (1,0), (1,0))
    val hash3 = Vector((1,0), (1,1), (1,1), (0,1), (0,1), (1,1))
    val hash4 = Vector((1,0), (1,1), (1,0), (0,1), (0,0), (1,0))
    fullHash.toArray should === (Array(hash1, hash2, hash3, hash4))
    
    val t1 = Map((1,0) -> Set(0,2,3), (1,1) -> Set(1))
    val t2 = Map((1,1) -> Set(0,1,2,3))
    val t3 = Map((1,1) -> Set(0,2), (1,0) -> Set(1,3))
    val t4 = Map((0,1) -> Set(0,2,3), (1,1) -> Set(1))
    val t5 = Map((0,1) -> Set(0,2), (1,0) -> Set(1), (0,0) -> Set(3))
    val t6 = Map((1,1) -> Set(0,2), (1,0) -> Set(1,3))
    val ht = Vector(t1, t2, t3, t4, t5, t6)
    Lsh.hashTable(fullHash) should === (ht)
  }
  
  it should "correctly build the tNN graph" in {
    val V = Vector(Vector(1.0,2.0), Vector(2.0,3.0), Vector(3.0,4.0), Vector(4.0,5.0))
    val I = Vector(Vector(3.0,2.0), Vector(1.0,4.0), Vector(1.0,5.0), Vector(4.0,3.0))
    val hv11 = Map((1,2) -> Set(2), (1,3) -> Set(0,3), (2,4) -> Set(1))
    val hv21 = Map((1,3) -> Set(0), (1,4) -> Set(2,3), (5,5) -> Set(1))
    val hv22 = Map((5,5) -> Set(0), (4,4) -> Set(1,2), (6,1) -> Set(3))
    val VHashTableCol = Vector(Vector(hv11), Vector(hv21, hv22))
    val hi1 = Vector(Vector((2,4)), Vector((1,2)), Vector((3,4)), Vector((1,3)))
    val hi2 = Vector(Vector((1,3), (1,4)), Vector((7,8), (5,5)), Vector((3,2), (5,5)), Vector((6,1), (4,4)))
    val IHashCol = Vector(hi1, hi2)
    val sim = (x: Vector[Double], y:Vector[Double]) => (x zip y).map{case (xi,yi) => xi * yi}.sum
    val thresh = Vector(16.0, 12.8)
    val t1 = 1
    val c = 0.8
    val G1 = Lsh.buildGraph(V, I, sim, VHashTableCol, IHashCol, thresh, c, t1)
    val Gtest1 = Map(0 -> Set(2), 2 -> Set(1), 3 -> Set(3))
    val t2 = 3
    val G2 = Lsh.buildGraph(V, I, sim, VHashTableCol, IHashCol, thresh, c, t2)
    val Gtest2 = Map(0 -> Set(2), 1 -> Set(3), 2 -> Set(1,3), 3 -> Set(3))
    Gtest1 should === (G1)
    Gtest2 should === (G2)
  }
  
  
  
  
}