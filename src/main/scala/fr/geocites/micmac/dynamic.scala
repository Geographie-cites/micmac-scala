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

import fr.geocites.micmac.context.MicMacState
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

  def randomDestination[M[_]: Monad: RNG](airport: Airport, network: Network): M[Airport] =
    implicitly[RNG[M]].rng.map { rng =>
      val neighbours = Network.neighbours(network, airport.index)
      val selected = rng.nextInt(neighbours.size)
      network.airports(neighbours(selected))
    }

  def planeDepartures[M[_]: Monad: RNG: Step](
    planeCapacity: Int,
    destination: (Airport, Network) => M[Airport],
    buildSIR: (Double, Double, Double) => SIR) = Kleisli[M, MicMacState, MicMacState] { modelState =>
    def departures(state: MicMacState): M[Vector[(Airport, List[Plane])]] =
      state.network.airports.traverseU {
        airport =>
          def buildPlane(s: Int, i: Int, r: Int) =
            for {
              step <- implicitly[Step[M]].get
              dest <- destination(airport, state.network)
            } yield Plane(
              capacity = planeCapacity,
              sir = buildSIR(s, i, r),
              origin = airport.index,
              destination = dest.index,
              departureStep = step
            )

          fillPlanes[M](airport, planeCapacity, buildPlane)
      }

    for {
      v <- departures(modelState)
    } yield
      ((MicMacState.network composeLens Network.airports).set(v.map(_._1)) andThen
        MicMacState.flyingPlanes.modify(_ ++ v.flatMap(_._2))) (modelState)
  }

  def planeArrivals[M[_]: Monad: Step](planeSpeed: Double) = Kleisli[M, MicMacState, MicMacState] { modelState =>
    val airports = (MicMacState.network composeLens Network.airports).get(modelState)

    def isArrived(plane: Plane, step: Long) = {
      val from = airports(plane.origin)
      val to =  airports(plane.destination)
      val distance = math.sqrt(math.pow(to.x - from.x, 2) + math.pow(to.y - from.y, 2))
      val flightDuration = distance / planeSpeed
      flightDuration >= (step - plane.departureStep)
    }

    def dispatch(airports: Vector[Airport], planes: List[Plane]): Vector[Airport] =
      planes match {
        case Nil => airports
        case plane :: t =>
          def flightSIR = (Plane.sir composeLens SIR.vector).get(plane)
          def newAirports =
            (index[Vector[Airport], Int, Airport](plane.destination)
              composeLens Airport.sir
              composeLens SIR.vector).modify { airportSIR => (airportSIR zip flightSIR).map { case(a, b) => a + b} } (airports)

          dispatch(newAirports, t)
      }

     implicitly[Step[M]].get.map { step =>
       val (arrived, inFlight) = MicMacState.flyingPlanes.get(modelState).span(isArrived(_, step))
       def airports = (MicMacState.network composeLens Network.airports).get(modelState)
       def newAirports = dispatch(airports, arrived.toList)
       (MicMacState.flyingPlanes.set(inFlight) andThen
         (MicMacState.network composeLens Network.airports).set(newAirports)) (modelState)
    }
  }

  def fillPlanes[M[_]: Monad: RNG](
    airport: Airport,
    planeCapacity: Int,
    buildPlane: (Int, Int, Int) => M[Plane]): M[(Airport, List[Plane])] = {
      def multinomial(v: Vector[Double], d: Double, i: Int = 0): Int =
        if(d <= v(i)) i else multinomial(v, d - v(i), i + 1)

      def fillPlane(airport: Airport, plane: Plane): M[(Airport, Plane)] =
        if (Plane.passengers(plane) >= plane.capacity) (airport, plane).point[M]
        else implicitly[RNG[M]].rng >>= { rng =>
          val selected = multinomial(Airport.stock.get(airport), rng.nextDouble * Airport.population(airport))
          def updateAirport = (Airport.stock composeOptional index(selected)).modify(_ - 1) andThen Airport.populationToFly.modify(_ - 1)
          val newAirport = updateAirport(airport)
          val newPlane = (Plane.stock composeOptional index(selected)).modify(_ + 1)(plane)
          fillPlane(newAirport, newPlane)
        }

      def fillPlanes(airport: Airport, planes: List[Plane] = List.empty): M[(Airport, List[Plane])] =
        // Plane should be full to leave
        if(Airport.populationToFly.get(airport) < planeCapacity) (airport, planes).point[M]
        else
          for {
            plane <-  buildPlane(0, 0, 0)
            filled <- fillPlane(airport, plane)
            (newAirport, newPlane) = filled
            res <- fillPlanes(newAirport, newPlane :: planes)
          } yield res

      fillPlanes(airport)
    }

  def updateStep[M[_]: Monad](implicit step: Step[M]) = step.modify(_ + 1)
  def stopAfter[M[_]: Monad](s: Long)(implicit step: Step[M]) = step.get.map { _ >= s }

}
