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

import scala.util.Random
import scalaz._
import Scalaz._
import network._
import context._
import monocle.std.all._
import monocle.function._
import dynamic._
import concurrent.duration._
import sir._
import util._
import observable._
import stop._

object Test extends App {


  val territory = Territory(1000, 1000)
  val nodes = 100
  val edges = 100
  val populationByNode = 10000
  val alpha = 0.2
  val beta = 0.5
  val planeCapacity = 80
  val planeSpeed = 0.5

  val airportIntegrator = integrator(1, 0.01)
  val planeIntegrator = integrator(1, 0.01)

  def sir(s: Double, i: Double, r: Double) =
    SIR(s = s, i = i, r = r,  alpha = alpha, beta = beta)

  val populationToFly =
    dynamic.populationToFly(
      sir = sir(s = populationByNode - 1, i = 1, r = 0),
      integrator = airportIntegrator,
      epidemyDuration = 60 days,
      epsilon = 0.001,
      mobilityRate = 0.012,
      nbAirports = nodes
    )

  val airports =
    randomAirports[Context](
      territory,
      Airport(_,_,_,  sir(s = populationByNode - 1, i = 1, r = 0), populationToFly / nodes),
      nodes
    )

  val world = randomNetwork[Context](edges).map( w => MicMacState(w, Vector.empty))

  def evolve[M[_]: Monad: RNG: Step: Observable] = {
    def updateAirportSIR = Kleisli[M, MicMacState, MicMacState] { state =>
      dynamic.updateSIRs[MicMacState](
        airportIntegrator,
        MicMacState.network composeTraversal Network.airportsTraversal composeLens Airport.sir
      )(state).point[M]
    }

    def updatePlaneSIR = Kleisli[M, MicMacState, MicMacState] { state =>
      dynamic.updateSIRs[MicMacState](
        planeIntegrator,
        MicMacState.flyingPlanes composeTraversal Each.each composeLens Plane.sir
      )(state).point[M]
    }

    updateAirportSIR andThen updatePlaneSIR andThen
     dynamic.planeDepartures[M](
       planeCapacity = planeCapacity,
       destination = dynamic.randomDestination[M],
       buildSIR = sir) andThen
      dynamic.planeArrivals[M](planeSpeed) andThen
      updateMaxStepI[M] andThen updateStep[M]
  }


  val initialise = airports >>= world

  val simulation =
    initialise >>=
      context.run(
        evolve,
        or(endOfEpidemy[Context], stopAfter[Context](10000))
      )

  val initialState = SimulationState(0, new Random(42), None)

//  println(populationToFly)
//  println((initialise >>= simulation).eval(initialState))
//
  println(simulation.run(initialState))
//
//
//  val network  = (Kleisli { _: Any => airports } andThen world).apply().eval(initialState)
//
//  println(evolve(network))




//
//  println(.map(evolution))

  //println(airports.map { world.run }.eval(initialState))



}
