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
        case get() => step
        case increment() =>
          step += 1
          step
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

      //FIXME use onion
      def interpret[_] = {
        case get() => state.getOrElse(throw new RuntimeException("state as not been set"))
        case set(v) => state = Some(v)
      }
    }
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
        case getMaxIStep() => maxIStep
        case setMaxIStep(s) => maxIStep = s
        case getInfectionStep() => infectionStep
        case setInfectionStep(v) => infectionStep = v
      }
    }

  }


  @Lenses case class MaxIStep(step: Long, value: Double)

  @dsl trait Observable[M[_]] {
    def getMaxIStep: M[Option[MaxIStep]]
    def setMaxIStep(v: Option[MaxIStep]): M[Unit]
    def getInfectionStep: M[Vector[Option[Long]]]
    def setInfectionStep(v: Vector[Option[Long]]): M[Unit]
  }

  type DSL =
    Step.DSL :||:
    Observable.DSL :||:
    ModelState.DSL :||:
    Random.DSL :||:
    Log.DSL

  val DSL = freek.DSL.Make[DSL]

  type Context[T] = Free[DSL.Cop, T]

  def interpreter(seed: Long) =
    Step.interpreter :&:
      Observable.interpreter :&:
      Random.interpreter(seed) :&:
      ModelState.interpreter :&:
      Log.interpreter

  implicit def random = Random.impl[DSL]
  implicit def log = Log.impl[DSL]
  implicit def step = Step.impl[DSL]
  implicit def modelState = ModelState.impl[DSL]
  implicit def observable = Observable.impl[DSL]

}
