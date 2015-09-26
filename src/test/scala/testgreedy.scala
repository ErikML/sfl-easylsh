package sfl.test

import sfl.Sfl
import org.scalatest.{FlatSpec, Matchers}
import sampler.DeterministicSampler
import scala.collection.parallel.immutable.ParSet

class GreedyTest extends FlatSpec with Matchers {
  behavior of "stochastic greedy"
  val v0 = Vector(1.0, 0.3)
  val v1 = Vector(0.3, 1.0)
  val v2 = Vector(0.5, 1.1)
  val v3 = Vector(1.2, 0.2)
  val v4 = Vector(0.1, 0.2)
  val v5 = Vector(0.5, 0.7)
  val V = Vector(v0, v1, v2, v3, v4, v5)
  val samples = List(Set(0,2,4), Set(0,4,5), Set(1,3,5))
  val eps = 1.0 / 3.0
  it should "choose the correct elements based on the random choices" in {
    Sfl.stochasticGreedy(V, 1, eps, DeterministicSampler(5, samples)) should === (Set(2))
    Sfl.stochasticGreedy(V, 2, eps, DeterministicSampler(5, samples)) should === (Set(2, 0))
    Sfl.stochasticGreedy(V, 3, eps, DeterministicSampler(5, samples)) should === (Set(2, 0, 3))
  }
  
  it should "choose the correct elements with the graph optimization" in {
    val n0 = ParSet((0, 1.09) ,(3, 1.26))
    val n1 = ParSet((1, 1.09),(2, 1.25), (4, 0.23) ,(5, 0.85))
    val n2 = ParSet((0, 0.83),(1, 1.25) ,(2, 1.46))
    val n3 = ParSet((3, 1.48) ,(5, 0.74))
    val n4 = ParSet[(Int, Double)]()
    val n5 = ParSet((1, 0.85) ,(2, 1.02) ,(4, 0.19))
    val graph = Vector(n0, n1, n2, n3, n4, n5)
    Sfl.stochasticGreedy(V, 1, eps, graph, false, DeterministicSampler(5, samples)) should === (Set(2))
    Sfl.stochasticGreedy(V, 2, eps, graph, false, DeterministicSampler(5, samples)) should === (Set(2, 0))
    Sfl.stochasticGreedy(V, 3, eps, graph, false, DeterministicSampler(5, samples)) should === (Set(2, 0, 1))
  }
  
}

