package sampler

import scala.util.Random

object Sampler {
  def apply(startSize: Int) = {
    new Sampler(startSize)
  }
}

object DeterministicSampler {
  def apply(startSize: Int, samples: List[Set[Int]]) = {
    new DeterministicSampler(startSize, samples)
  }
}
  
class Sampler(startSize: Int) {
  
  val arr = Array((0 to this.startSize - 1):_*)
  var currSize = this.startSize
  var prev_seen = Map[Int, Int]()
  
  def subset(s: Int): Set[Int] = {
    for(i <- 0 to s-1) {
      val m = Random.nextInt(currSize-i)
      val copy = this.arr(m)
      this.arr(m) = this.arr(currSize-i-1)
      this.arr(currSize-i-1) = copy
    }
    this.prev_seen = (this.currSize - s to this.currSize - 1).map(i => (this.arr(i), i)).toMap
    this.prev_seen.keySet
  }
  
  def removePrevSeenElem(id: Int) = {
    this.arr(this.prev_seen(id)) = this.arr(this.currSize-1)
    this.currSize -= 1
  }
}

class DeterministicSampler(val startSize:Int, var samples: List[Set[Int]]) extends Sampler(startSize) {
  
  override def subset(s: Int): Set[Int] = {
    val head = samples.head
    this.samples = samples.tail
    head
  }
  
  override def removePrevSeenElem(id: Int) = {}
  
}