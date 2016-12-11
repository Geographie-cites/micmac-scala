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

import network._
import monocle.std.all._
import monocle.function._
import dynamic._
import concurrent.duration._
import sir._
import cats._
import cats.data._
import cats.implicits._
import cats.free._
import context._
import observable._
import stop._
import freedsl.log.Log

object Test extends App {

  val territory = Territory(1000, 1000)
  val nodes = 50
  val edges = 500
  val populationByNode = 4000
  val alpha = 0.2
  val beta = 0.5
  val planeCapacity = 80
  val planeSpeed = 0.5

  val airportIntegrator: Integrator = integrator.integrateSIR(0.01)
  val planeIntegrator: Integrator = integrator.integrateSIR(0.01)

  def sir(s: Double, i: Double, r: Double) =
    SIR(s = s, i = i, r = r,  alpha = alpha, beta = beta)

  val populationToFly =
    dynamic.populationToFly(
      sir = sir(s = populationByNode - 1, i = 1, r = 0),
      integrator = airportIntegrator,
      epidemyDuration = 60 days,
      epsilon = 0.001,
      mobilityRate = 0.017,
      nbAirports = nodes
    )

  import context.context._

  def airports =
    randomAirports[M](
      territory,
      (index, x, y, infected) =>
        Airport(
          index,
          x,
          y,
          sir(s = populationByNode - infected, infected, r = 0),
          populationToFly
        ),
      nodes
    )

  def initState = for {
    a <- airports
    network <- randomNetwork[M](edges, a)
    _ <- implicitly[ModelState[M]].set(MicMacState(network, Vector.empty))
  } yield ()

  def evolve = {
    def stateModifier = {
      def modelState = implicitly[ModelState[M]]
      freedsl.modifier(modelState.get, modelState.set)
    }

    def updateAirportSIR =
      stateModifier modify {
        dynamic.updateSIRs[MicMacState](
          airportIntegrator,
          MicMacState.network composeTraversal
            Network.airportsTraversal composeLens
            Airport.sir
        )
      }

    def updatePlaneSIR =
      stateModifier modify {
        dynamic.updateSIRs[MicMacState](
          planeIntegrator,
          MicMacState.flyingPlanes composeTraversal
            Each.each composeLens
            Plane.sir
        )
      }

    for {
      _ <- updateAirportSIR
      _ <- updatePlaneSIR
      _ <- planeDepartures[M](
         planeCapacity = planeCapacity,
         populationToFly = populationToFly,
         destination = dynamic.randomDestination[M],
         buildSIR = sir)
      _ <- planeArrivals[M](planeSpeed)
      _ <- updateMaxStepI[M]
      s <- updateStep[M]
      _ <- updateInfectedNodes[M]
      _ <- implicitly[Log[M]].print(s"Step $s")
    } yield ()
  }

  def end = or(endOfEpidemy[M], stopAfter[M](20000))
  def loop = evolve.until(end)

  def simulation =
    for {
      _ <- initState
      _ <- loop
      ind <- observable.indicators[M]
    } yield ind

  println(result(simulation, interpreter(42)))

}
