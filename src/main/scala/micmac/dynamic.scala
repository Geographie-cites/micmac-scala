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

package micmac



import squants.time._
import monocle.std.vector._
import monocle.function.all._


import cats.free._
import cats.implicits._
import cats._
import freek._

import context._
import integrator._
import freedsl.random._

object dynamic {

  type BuildSIR = (Double, Double, Double) => SIR

  def populationToFly(
    sir: SIR,
    integrator: Integrator,
    epidemyDuration: Time,
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

    def totalPopulationToFly = (sir.total * nbAirports) * mobilityRate * epidemyDuration.toDays

    (totalPopulationToFly / nbSteps) / nbAirports
  }

  def updateSIRs[T](integrator: Integrator, sir: monocle.Traversal[T, SIR])(t: T) =
    sir.modify(integrator)(t)

  def randomDestination[M[_]: Monad](airport: Airport, network: Network)(implicit rng: Random[M]) = {
    val neighbours = Network.neighbours(network, airport.index)
    for {
      selected <- rng.nextInt(neighbours.size)
    } yield network.airports(neighbours(selected))
  }

  def planeDepartures[M[_]: Monad](
    planeCapacity: Int,
    populationToFly: Double,
    destination: (Airport, Network) => M[Airport],
    buildSIR: BuildSIR)(implicit step: Step[M], state: ModelState[M], rng: Random[M]) = {

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

        val newState = (setNewAirports andThen updatePopulationToFly andThen addDepartingPlanes) (modelState)
        (MicMacState.network composeTraversal Network.airportsTraversal ).getAll(newState).foreach {
          a => assert(a.sir.total >= 0, s"${a.sir.total}")
        }
        newState
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
      (step - plane.departureStep) >= flightDuration
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
      buildPlane: (Int, Int, Int) => M[Plane])(implicit rng: Random[M]) = {
        def multinomial(v: Vector[Int]) = {
          def multinomial0(d: Int, i: Int): Int =
            v.lift(i) match {
              case Some(v) => if (d < v) i else multinomial0(d - v, i + 1)
              case None => i - 1
            }

          rng.nextInt(v.sum).map(d => multinomial0(d, 0))
        }


        def discreteStock(airport: Airport) = Airport.stock.get(airport).map { math.floor(_).toInt }

        def fillPlane(airport: Airport, plane: Plane) =
          Monad[M].tailRecM((airport, plane)) { case(airport, plane) =>
            def updateAirport(selected: Int) = (Airport.stock composeOptional index(selected)).modify(_ - 1) andThen Airport.populationToFly.modify(_ - 1)

            def boardPassenger =
              for {
                selected <- multinomial(discreteStock(airport))
                newAirport = updateAirport(selected)(airport)
                newPlane = (Plane.stock composeOptional index(selected)).modify(_ + 1)(plane)
              } yield Left((newAirport, newPlane)): Either[(Airport, Plane), (Airport, Plane)]

            if (Plane.passengers(plane) >= plane.capacity) (Right((airport, plane)): Either[(Airport, Plane), (Airport, Plane)]).pure[M]
            else boardPassenger
          }


        def fillPlanes(airport: Airport, planes: List[Plane] = List.empty): M[(Airport, List[Plane])] =
         // Plane should be full to leave and airport should contain enough passengers
          if (Airport.populationToFly.get(airport) < planeCapacity || discreteStock(airport).sum < planeCapacity) (airport, planes).pure[M]
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
