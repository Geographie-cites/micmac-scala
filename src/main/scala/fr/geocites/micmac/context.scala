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


import monocle.macros.Lenses

import scala.util.Random
import scalaz._
import Scalaz._

object context {

  @Lenses case class MicMacState(network: Network, flyingPlanes: Vector[Plane])
  @Lenses case class SimulationState(step: Long, rng: Random)

  type Context[X] = State[SimulationState, X]

  /*implicit def modelState = new ModelState[Context, MicMacState] {
    override def get: Context[MicMacState] = State.get[SimulationState].map(_.micMacState)
    override def set(state: MicMacState): Context[Unit] = State.modify[SimulationState](SimulationState.micMacState.set(state))
  }*/

  implicit def sRNG = new RNG[Context] {
    override def rng: Context[Random] = State.get[SimulationState].map(_.rng)
  }

  implicit def sStep = new Step[Context] {
    override def modify(f: (Long) => Long): Context[Unit] = State.modify[SimulationState](SimulationState.step.modify(f))
    override def get: Context[Long] = State.gets[SimulationState, Long](_.step)
  }

  def run[T](step: Kleisli[Context, T, T], stop: Context[Boolean]): Kleisli[Context, T, T] = {
    def runStep(result: T, s: SimulationState): (SimulationState, T) = {
      val (s1, stop1) = stop.run(s)

      if(stop1) (s1, result)
      else {
        val (s2, result2) = step.run(result).run(s1)
        runStep(result2, s2)
      }
    }

    Kleisli[Context, T, T] { t =>
      State { s: SimulationState =>
        val (s1, res1) = step.run(t).run(s)
        runStep(res1, s1)
      }
    }
  }

}
