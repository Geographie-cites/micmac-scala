/**
  * Created by Romain Reuillon on 19/04/16.
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

import fr.geocites.micmac.context._

import scalaz._
import Scalaz._

object observable {

  def totalI(state: MicMacState) =
    state.flyingPlanes.map(_.sir.i).sum +
      state.network.airports.map(_.sir.i).sum

  def updateMaxStepI[M[_]: Monad](implicit obs: Observable[M], step: Step[M]) = Kleisli[M, MicMacState, MicMacState] { state: MicMacState =>
    val totalIValue = totalI(state)

    def update(step: Long) =
      obs.maxIStep.modify {
        case Some(current) =>
          if (totalIValue > current.value) Some(MaxIStep(step, totalIValue)) else Some(current)
        case None => Some(MaxIStep(step, totalIValue))
      }

    for {
      s <- step.step.get
      _ <- update(s)
    } yield state
  }

}
