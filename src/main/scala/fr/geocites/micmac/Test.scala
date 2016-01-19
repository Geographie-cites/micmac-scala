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
import monocle.std.vector._
import monocle.function._
import dynamic._
import concurrent.duration._
import sir._

object Test extends App {


  val territory = Territory(1000, 1000)
  val nodes = 100
  val populationByNode = 10000
  val alpha = 0.2
  val beta = 0.5

  val airportIntegrator = integrator(1, 0.01)

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
      4
    )

  val world = randomNetwork[Context](2)


  val allAirports = (Network.nodes composeTraversal Each.each)

  def evolve = Kleisli[Context, Network, Network] { network =>
    dynamic.evolve[Network](airportIntegrator, allAirports composeLens Airport.sir)(network).point[Context]
  }

  val initialise = airports >>= world

  val step: Kleisli[Context, Network, Network] = evolve andThen updateStep

  val simulation =
    context.run(
      step,
      stopAfter[Context](100)
    )

  val initialState = MicMacState(0, new Random(42))

  println(populationToFly)
  println((initialise >>= simulation).eval(initialState))

 // println(simulation.run())


  //val network  = (Kleisli { _: Any => airports } andThen world).apply().eval(initialState)

  //println(evolve(network))




//
//  println(.map(evolution))

  //println(airports.map { world.run }.eval(initialState))



}
