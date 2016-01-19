/**
  * Created by Romain Reuillon on 19/01/16.
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Affero General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
package fr.geocites.micmac

import fr.geocites.micmac.sir.Integrator

import scala.concurrent.duration.Duration
import scalaz._
import Scalaz._
import monocle.std.vector._
import monocle.function.all._

import sir._

object dynamic {

  def populationToFly(sir: SIR, integrator: Integrator, epidemyDuration: Duration, mobilityRate: Double, epsilon: Double, nbAirports: Int): Long = {
    def steps(sir: SIR, step: Int = 0): Int = {
      val newSir = integrator(sir)
      if (newSir.i < epsilon) step
      else steps(newSir, step + 1)
    }

    val nbSteps = steps(sir)
    def dt = epidemyDuration.toHours / nbSteps
    def totalPopulationToFly = (sir.s + sir.i + sir.r) * mobilityRate * dt
    (totalPopulationToFly / nbSteps).toLong
  }

  def epidemy[T](integrator: Integrator, sir: monocle.Traversal[T, SIR])(t: T) = sir.modify(integrator)(t)

  def updateStock(airport: Airport) = Airport.stockToFly.modify(_ + airport.populationToFly)

  def fillPlanes[M[_]: Monad: RNG](
    airport: Airport,
    planeSize: Int,
    buildPlane: (Int, Int, Int) => Plane) =
    for {
      rng <- implicitly[RNG[M]].rng
    } yield {
      if(airport.stockToFly < planeSize) (airport, Vector.empty)
      else {
        def multinomial(v: Vector[Int], d: Double, i: Int = 0): Int =
          if(d <= v(i)) i else multinomial(v, d - v(i), i + 1)

        def fillPlane(sir: Vector[Int], acc: Vector[Int] = Vector.fill(3)(0)): (Vector[Int], Plane) =
          if(acc.sum >= planeSize) (sir, buildPlane(acc(0), acc(1), acc(2)))
          else {
            val selected = multinomial(sir, rng.nextDouble * sir.sum)
            val newAcc = index[Vector[Int], Int, Int](selected).modify(_ + 1)(acc)
            val newSir = index[Vector[Int], Int, Int](selected).modify(_ - 1)(sir)
            fillPlane(newSir, newAcc)
          }


        def fillPlanes(sir: Vector[Int], stockToFly: Double, acc: List[Plane] = List.empty): (List[Plane], Vector[Int]) =
          if(stockToFly < planeSize) (acc, sir)
          else {
            val (newSir, plane) = fillPlane(sir)
            fillPlanes(newSir, stockToFly - plane.passengers, plane :: acc)
          }

        val initPopulations = SIR.vector.get(airport.sir).map(_.toInt)
        val (planes, newPopulation) = fillPlanes(initPopulations, airport.stockToFly)
        val deltaPopulation = (initPopulations zip newPopulation).map { case(i, n) => i - n }
        val newAirport = (Airport.sir composeLens SIR.vector).modify { p => (p zip deltaPopulation).map { case (p, d) => p - d } }(airport)
        (Airport.stockToFly.modify(_ - planes.map(_.passengers).sum)(newAirport), planes.toVector)
      }
    }

  def updateStep[M[_]: Monad: Step, T] = Kleisli[M, T, T] { t: T => implicitly[Step[M]].modify(_ + 1).map(_ => t) }

  def stopAfter[M[_]: Monad: Step](s: Long) = implicitly[Step[M]].get.map { _ >= s }

}
