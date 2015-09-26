package sampler.test

import sampler.Sampler
import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

class SamplerTest extends FlatSpec with Matchers {
  behavior of "sampler example"
  
  it should "correctly return the random samples based on an initial seed" in {
    Random.setSeed(37)
    val sampler = Sampler(4)
    sampler.subset(2) should === (Set(0,2))
    sampler.removePrevSeenElem(0)
    sampler.arr should === (Array(3,1,2,2))
    sampler.subset(2) should === (Set(2,3))
    sampler.removePrevSeenElem(2)
    sampler.arr should === (Array(1,3,3,2))
    sampler.subset(1) should === (Set(3))
  }
}