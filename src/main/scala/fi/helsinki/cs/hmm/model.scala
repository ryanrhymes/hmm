
package fi.helsinki.cs.hmm

import scala.math._
import scala.util.Random
import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions._


class Model(A: DenseMatrix[Double], g: DenseVector[Double]) {

    def test: DenseMatrix[Double] = { A }

    def sample(num: Int) = {
    	val q  = DenseVector.zeros[Int](num)
    	val o1 = DenseVector.zeros[Double](num)
	val tA = accumulate(A(*,::))
	val rd = Uniform(0.0,1.0)

	(1 until num).map(i  => {
	    val r = rd.draw
	    q(i)  = sum(tA(q(i-1), ::).t.map(x => { if (r > x) 1 else 0 }))
	    o1(i) = .2
	})

	(q,o1)
    }

}


object Main {
    def main(args: Array[String]) {
	val A = DenseMatrix((0.25,0.75),(0.70,0.30))
	val g = DenseVector.zeros[Double](2)
    	val m = new Model(A, g)
        println(m.sample(5))
    }
}