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

  def total(on: SIR => Double)(state: MicMacState) =
    state.flyingPlanes.map(p => on(p.sir)).sum + state.network.airports.map(a => on(a.sir)).sum

  def updateMaxStepI[M[_]: Monad](implicit obs: Observable[M], step: Step[M]) = Kleisli[M, MicMacState, MicMacState] { state: MicMacState =>
    val totalIValue = total(SIR.i.get)(state)

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

  case class Indicators(maxIStep: MaxIStep, infectedRatio: Double)

  def indicators[M[_]: Monad](implicit obs: Observable[M]) = Kleisli[M, MicMacState, Option[Indicators]] { state =>
    def infectedRatio = {
      def totalPopulation = total(_.total)(state)
      def recovered = total(_.r)(state)
      recovered / totalPopulation
    }

    for {
      maxIStep <- obs.maxIStep.get
    } yield maxIStep.map(mis => Indicators(mis, infectedRatio))

  }

  def updateInfectedNodes[M[_]: Monad](implicit obs: Observable[M], step: Step[M]) = Kleisli[M, MicMacState, MicMacState] { state =>
    def update(infectionSteps: Vector[Option[Long]], currentStep: Long) =
      infectionSteps zip (MicMacState.network composeLens Network.airports).get(state) map {
        case (is, a) =>
          def infected = a.sir.i > 0
          is orElse (if(infected) Some(currentStep) else None)
      }

    for {
      s <- step.step.get
      _ <- obs.infectionStep.modify(update(_, s))
    } yield state
  }


}
