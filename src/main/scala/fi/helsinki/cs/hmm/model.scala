
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
	    o1(i) = draw(q(i))
	})

	(q,o1)
    }


    def b(k: Double, j: Int) = {
    	// P(b(t)=k | q(t)=j)
	
    }


    def draw(j: Int) = { Exponential(g(j)).draw }

    def output(q: DenseVector[Int], o1: DenseVector[Double]) {
    	for (i <- 0 until q.length) {
    	    println(s"${q(i)}\t${o1(i)}")
    }
}


}


object Main {
    def main(args: Array[String]) {
	val A = DenseMatrix((.25, .75),(.70, .30))
	val g = DenseVector(.1, .5)
    	val m = new Model(A, g)

	val (q, o1) = m.sample(10000)
        m.output(q, o1)
    }
}