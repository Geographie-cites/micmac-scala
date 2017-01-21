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
package micmac

import cats._
import cats.implicits._

import freedsl.tool.modifier
import context._

object observable {

  def allSIR(state: MicMacState) =
    state.flyingPlanes.map(_.sir) ++ state.network.airports.map(_.sir)

  def updateMaxStepI[M[_]: Monad](implicit obs: Observable[M], state: ModelState[M], step: Step[M]) = {
    def update(step: Long, totalIValue: Double, airportIValues: Vector[Double]): M[Option[MaxIStep]] = {
      modifier(obs.getMaxIStep, obs.setMaxIStep) apply {
        case Some(current) =>
          val newTotalIValue = if (totalIValue > current.totalI.value) StepValue(step, totalIValue) else current.totalI
          val newInfectedByAirport =
            (current.infectedByAirport zip airportIValues).map { case(cur, av) =>
              if(av > cur.value) StepValue(step, av) else cur
            }
          Some(MaxIStep(newTotalIValue, newInfectedByAirport))
        case None => Some(MaxIStep(StepValue(step, totalIValue), airportIValues.map(StepValue(step, _))))
      }
    }

    for {
      state <- state.get
      totalIValue = allSIR(state).map(_.i).sum
      step <- step.get
      _ <- update(step, totalIValue, state.network.airports.map(_.sir.i))
    } yield ()
  }

  case class Indicators(maxIStep: Option[MaxIStep], infectedRatio: Double)

  def indicators[M[_]: Monad](implicit obs: Observable[M], stateM: ModelState[M]) = {
    def infectedRatio(state: MicMacState) = {
      def totalPopulation = allSIR(state).map(_.total).sum
      def recovered = allSIR(state).map(_.r).sum
      recovered / totalPopulation
    }

    for {
      state <- stateM.get
      maxIStep <- obs.getMaxIStep
    } yield Indicators(maxIStep, infectedRatio(state))
  }


  def updateInfectedNodes[M[_]: Monad](implicit obs: Observable[M], step: Step[M], stateM: ModelState[M]) = {
    def update(infectionSteps: Vector[Option[Long]], currentStep: Long, state: MicMacState) =
      infectionSteps zip (MicMacState.network composeLens Network.airports).get(state) map {
        case (is, a) =>
          def infected = a.sir.i > 0
          is orElse (if(infected) Some(currentStep) else None)
      }

    for {
      step <- step.get
      state <- stateM.get
      _ <- modifier(obs.getInfectionStep, obs.setInfectionStep) apply (update(_, step, state))
    } yield ()
  }


}
