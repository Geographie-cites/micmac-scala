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

import cats.Monad
import cats.implicits._
import monocle.function._
import monocle.macros.Lenses
import monocle.std.vector._

//import scala.util.Random
import simulacrum._

package object micmac {

  def modifier[F[_]: Monad, T](get: F[T], set: T => F[Unit]) = new {
    def modify(f: T => T) =
      for {
        v <- get
        nv = f(v)
        _ <- set(nv)
      } yield nv

    def apply(f: T => T) = modify(f)
  }

  trait RNG[M[_]] {
    def nextDouble: M[Double]
    def nextInt(n: Int): M[Int]
  }

  trait Step[M[_]] {
    def get: M[Int]
    def increment: M[Int]
  }

  trait ModelState[M[_]] {
    def get: M[MicMacState]
    def set(s: MicMacState): M[Unit]
  }

  @Lenses case class MaxIStep(step: Long, value: Double)

  trait Observable[M[_]] {
    def getMaxIStep: M[Option[MaxIStep]]
    def setMaxIStep(v: Option[MaxIStep]): M[Unit]
    def getInfectionStep: M[Vector[Option[Long]]]
    def setInfectionStep(v: Vector[Option[Long]]): M[Unit]
  }

  trait Log[M[_]] {
    def print(s: String): M[Unit]
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


  implicit class MonadDecorator[M[_]: Monad, A](m: M[A]) {
    def until(end: M[Boolean]): M[A] = {
      def stop(a: A): Either[Unit, A] = Right(a)
      val continue: Either[Unit, A] = Left(Unit)

      def loop = Monad[M].tailRecM[Unit, A](Unit) { i =>
        val comp =
          for {
            a <- m
            b <- end
          } yield (b, a)

        comp.map { case (e, a) => (if(e) stop(a) else continue) }
      }

      loop
    }
  }

}
