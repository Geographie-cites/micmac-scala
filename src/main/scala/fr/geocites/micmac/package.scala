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
package fr.geocites

import monocle.function._
import monocle.macros.Lenses
import monocle.std.vector._

import scala.util.Random
import simulacrum._
import scalaz._
import Scalaz._


package object micmac {

  type Field[M[_], A] = (A => A) => M[A]

  implicit class getSetModDecorator[M[_], A](f: Field[M, A]) {
    def get = f(identity)
    def set(a: A) = f(_ => a)
    def modify(m: A => A) = f(m)
  }

  @typeclass trait RNG[M[_]] {
    def rng: M[Random]
  }

  @typeclass trait Step[M[_]] {
    def step: Field[M, Long]
  }

  @Lenses case class MaxIStep(step: Long, value: Double)

  @typeclass trait Observable[M[_]] {
    def maxIStep: Field[M, Option[MaxIStep]]
    def infectionStep: Field[M, Vector[Option[Long]]]
  }

  case class Territory(length: Int, width: Int) {
    def area = length * width
  }

  object SIR {
    def vector =
      monocle.Lens[SIR, Vector[Double]](sir => Vector(sir.s, sir.i, sir.r))(v => sir => sir.copy(s = v(0), i = v(1), r = v(2)))
  }

  @Lenses case class SIR(s: Double, i: Double, r: Double, alpha: Double, beta: Double) {
    def normalisedBeta =  beta / total
    def total = s + i + r
  }


  type Edge = (Int, Int)

  object Network {
    def neighbours(network: Network, index: Int) =
      network.routes.filter(_._1 == index).map{_._2}

    def airportsTraversal = (Network.airports composeTraversal Each.each)
  }

  @Lenses case class Network(airports: Vector[Airport], routes: Vector[Edge])

  object Airport {
    def stock = Airport.sir composeLens SIR.vector
    def population(a: Airport) = Airport.stock.get(a).sum
  }

  @Lenses case class Airport(index: Int, x: Double, y: Double, sir: SIR, populationToFly: Double)

  object Plane {
    def stock = Plane.sir composeLens SIR.vector
    def passengers(p: Plane) = Plane.stock.get(p).sum
  }

  @Lenses case class Plane(capacity: Int, sir: SIR, origin: Int, destination: Int, departureStep: Long)

  object MicMacState {
    def flyingPlaneTraversal = (MicMacState.flyingPlanes composeTraversal Each.each)
  }

  @Lenses case class MicMacState(network: Network, flyingPlanes: Vector[Plane])

}
