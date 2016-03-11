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
import monocle.function.At.at
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

  def updateSIRs[T](integrator: Integrator, sir: monocle.Traversal[T, SIR])(t: T) =
    sir.modify(integrator)(t)



  def fillPlanes[M[_]: Monad: RNG](
    airport: Airport,
    planeSize: Int,
    buildPlane: (Int, Int, Int) => Plane): M[(Airport, List[Plane])] =
    for {
      rng <- implicitly[RNG[M]].rng
    } yield {
      def multinomial(v: Vector[Double], d: Double, i: Int = 0): Int =
        if(d <= v(i)) i else multinomial(v, d - v(i), i + 1)

      def fillPlane(airport: Airport, plane: Plane = buildPlane(0, 0, 0)): (Airport, Plane) =
        if (Plane.passengers(plane) >= planeSize) (airport, plane)
        else {
          val selected = multinomial(Airport.stock.get(airport), rng.nextDouble * Airport.population(airport))
          def updateAirport = (Airport.stock composeOptional index(selected)).modify(_ - 1) andThen Airport.populationToFly.modify(_ - 1)
          val newAirport = updateAirport(airport)
          val newPlane = (Plane.stock composeOptional index(selected)).modify(_ + 1)(plane)
          fillPlane(newAirport, newPlane)
        }

      def fillPlanes(airport: Airport, planes: List[Plane] = List.empty): (Airport, List[Plane]) =
        // Plane should be full to leave
        if(Airport.populationToFly.get(airport) < planeSize) (airport, planes)
        else {
          val (newAirport, plane) = fillPlane(airport)
          fillPlanes(newAirport, plane :: planes)
        }

      fillPlanes(airport)
    }

  def updateStep[M[_]: Monad: Step, T] = Kleisli[M, T, T] { t: T => implicitly[Step[M]].modify(_ + 1).map(_ => t) }

  def stopAfter[M[_]: Monad: Step](s: Long) = implicitly[Step[M]].get.map { _ >= s }

}
