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

import freedsl.dsl._
import freedsl.random._
import freedsl.log._

import cats._
import cats.free._
import freek._

import monocle.macros.Lenses

object context {

  object Step {
    def interpreter = new Interpreter[Id] {
      var step = 0

      def interpret[_] = {
        case get() => Right(step)
        case increment() =>
          step += 1
          Right(step)
      }
    }
  }

  @dsl trait Step[M[_]] {
    def get: M[Int]
    def increment: M[Int]
  }

  object ModelState {
    def interpreter = new Interpreter[Id] {
      var state: Option[MicMacState] = None

      def interpret[_] = {
        case get() => state match {
          case Some(s) => Right(s)
          case None => Left(StateNotSet)
        }
        case set(v) => Right(state = Some(v))
      }
    }

    case object StateNotSet extends Error
  }

  @dsl trait ModelState[M[_]] {
    def get: M[MicMacState]
    def set(s: MicMacState): M[Unit]
  }

  object Observable {
    def interpreter = new Interpreter[Id] {
      var maxIStep: Option[MaxIStep] = None
      var infectionStep: Vector[Option[Long]] = Vector.empty

      def interpret[_] =  {
        case getMaxIStep() => Right(maxIStep)
        case setMaxIStep(s) => Right(maxIStep = s)
        case getInfectionStep() => Right(infectionStep)
        case setInfectionStep(v) => Right(infectionStep = v)
      }
    }

  }

  case class StepValue(step: Long, value: Double)
  @Lenses case class MaxIStep(totalI: StepValue, infectedByAirport: Vector[StepValue])

  @dsl trait Observable[M[_]] {
    def getMaxIStep: M[Option[MaxIStep]]
    def setMaxIStep(v: Option[MaxIStep]): M[Unit]
    def getInfectionStep: M[Vector[Option[Long]]]
    def setInfectionStep(v: Vector[Option[Long]]): M[Unit]
  }

  def interpreter(seed: Long) =
    Step.interpreter :&:
      Observable.interpreter :&:
      Random.interpreter(seed) :&:
      ModelState.interpreter :&:
      Log.interpreter

  val merged = freedsl.dsl.merge(Step, Observable, Random, ModelState, Log)

}
