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


//import context._
//
import scala.concurrent.duration._
//
import monocle.std.vector._
import monocle.function.all._


import cats.free._
import cats.implicits._
import cats._
import freek._
import sir._

object dynamic {

  def populationToFly(
    sir: SIR,
    integrator: Integrator,
    epidemyDuration: Duration,
    mobilityRate: Double,
    epsilon: Double,
    nbAirports: Int): Double = {

    def steps(sir: SIR, step: Int = 0): Int = {
      val newSir = integrator(sir)
      if (newSir.i <= epsilon) step
      else steps(newSir, step + 1)
    }

    val nbSteps = steps(sir)
    //def dt = epidemyDuration.toHours.toDouble / nbSteps

    def totalPopulationToFly = (sir.total * nbAirports) * mobilityRate * epidemyDuration.toDays.toDouble

    (totalPopulationToFly / nbSteps) / nbAirports
  }

  def updateSIRs[T](integrator: Integrator, sir: monocle.Traversal[T, SIR])(t: T) =
    sir.modify(integrator)(t)

  def randomDestination[M[_]: Monad](airport: Airport, network: Network)(implicit rng: RNG[M]) = {
    val neighbours = Network.neighbours(network, airport.index)
    for {
      selected <- rng.nextInt(neighbours.size)
    } yield {
      val neighbours = Network.neighbours(network, airport.index)
      network.airports(neighbours(selected))
    }
  }

  def planeDepartures[M[_]: Monad](
    planeCapacity: Int,
    populationToFly: Double,
    destination: (Airport, Network) => M[Airport],
    buildSIR: (Double, Double, Double) => SIR)(implicit step: Step[M], state: ModelState[M], rng: RNG[M]) = {

    def departures(state: MicMacState) =
      state.network.airports.traverseU {
        airport =>
          def buildPlane(s: Int, i: Int, r: Int) =
            for {
              step <- step.get
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

    def departed(modelState: MicMacState) =
      for {
        v <- departures(modelState)
      } yield {
        val (newAirports, departedPlanes) = v.unzip

        def setNewAirports = (MicMacState.network composeLens Network.airports) set newAirports
        def updatePopulationToFly = (MicMacState.network composeTraversal Network.airportsTraversal composeLens Airport.populationToFly) modify (_ + populationToFly)
        def addDepartingPlanes = MicMacState.flyingPlanes modify(_ ++ departedPlanes.flatten)

        (setNewAirports andThen updatePopulationToFly andThen addDepartingPlanes) (modelState)
      }

    for {
      s <- state.get
      ns <- departed(s)
      _ <- state.set(ns)
    } yield ()
  }

  def planeArrivals[M[_]: Monad](planeSpeed: Double)(implicit step: Step[M], state: ModelState[M]) = {

    def isArrived(plane: Plane, step: Long, airports: Vector[Airport]) = {
      val from = airports(plane.origin)
      val to =  airports(plane.destination)
      val distance = math.sqrt(math.pow(to.x - from.x, 2) + math.pow(to.y - from.y, 2))
      val flightDuration = distance / planeSpeed
      flightDuration >= (step - plane.departureStep)
    }

    def dispatch(airports: Vector[Airport], arrived: List[Plane]): Vector[Airport] =
      arrived match {
        case Nil => airports
        case plane :: tail =>
          def flightSIR = (Plane.sir composeLens SIR.vector) get plane
          def newAirports =
            (index[Vector[Airport], Int, Airport](plane.destination)
              composeLens Airport.sir
              composeLens SIR.vector).modify { airportSIR => (airportSIR zip flightSIR).map { case(a, b) => a + b} } (airports)

          dispatch(newAirports, tail)
      }

    def afterArrival(modelState: MicMacState, step: Int) = {
      val airports = (MicMacState.network composeLens Network.airports) get modelState
      val (arrived, inFlight) = MicMacState.flyingPlanes.get(modelState).span(isArrived(_, step, airports))
      def newAirports = dispatch(airports, arrived.toList)
      (MicMacState.flyingPlanes.set(inFlight) andThen
        (MicMacState.network composeLens Network.airports).set(newAirports)) (modelState)
    }

      for {
        modelState <- state.get
        step <- step.get
        newState = afterArrival(modelState, step)
        _ <- state.set(newState)
      } yield ()
  }

  def fillPlanes[M[_]: Monad](
      airport: Airport,
      planeCapacity: Int,
      buildPlane: (Int, Int, Int) => M[Plane])(implicit rng: RNG[M]) = {
        def multinomial(v: Vector[Double], d: Double, i: Int = 0): Int =
          if (d <= v(i)) i else multinomial(v, d - v(i), i + 1)

        def fillPlane(airport: Airport, plane: Plane): M[(Airport, Plane)] = {
          def boardPassenger(d: Double) = {
            val selected = multinomial(Airport.stock.get(airport), d * Airport.population(airport))
            def updateAirport = (Airport.stock composeOptional index(selected)).modify(_ - 1) andThen Airport.populationToFly.modify(_ - 1)
            val newAirport = updateAirport(airport)
            val newPlane = (Plane.stock composeOptional index(selected)).modify(_ + 1)(plane)
            fillPlane(newAirport, newPlane)
          }

          if (Plane.passengers(plane) >= plane.capacity) (airport, plane).pure[M]
          else
            for {
              d <- rng.nextDouble
              p <- boardPassenger(d)
            } yield p
        }

        def fillPlanes(airport: Airport, planes: List[Plane] = List.empty): M[(Airport, List[Plane])] =
        // Plane should be full to leave
          if (Airport.populationToFly.get(airport) < planeCapacity) (airport, planes).pure[M]
          else
            for {
              plane <- buildPlane(0, 0, 0)
              filled <- fillPlane(airport, plane)
              (newAirport, newPlane) = filled
              res <- fillPlanes(newAirport, newPlane :: planes)
            } yield res

        fillPlanes(airport)
      }

  def updateStep[M[_]: Monad](implicit step: Step[M]) = step.increment

}
