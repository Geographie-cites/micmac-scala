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


object Test extends App {


  val territory = Territory(100, 100)

  val airports =
    randomAirports[Context](
      territory,
      Airport(_,_,_, SIR(s = 1000, i = 1, r = 0,  alpha = 0.2, beta = 0.5)),
      4
    )

  val world = randomNetwork[Context](2)

  val airportIntegrator = sir.integrator(1, 0.01)

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

  println((initialise >>= simulation).eval(initialState))

 // println(simulation.run())


  //val network  = (Kleisli { _: Any => airports } andThen world).apply().eval(initialState)

  //println(evolve(network))




//
//  println(.map(evolution))

  //println(airports.map { world.run }.eval(initialState))



}
