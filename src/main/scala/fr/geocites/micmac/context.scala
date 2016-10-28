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


import freek._
import cats.free._
import cats._

object context {

  object Step {
    sealed trait Instruction[A]
    final case object Get extends Instruction[Int]
    final case object Increment extends Instruction[Int]

    type DSL = Instruction :|: NilDSL
    val DSL = freek.DSL.Make[DSL]

    def interpreter = new (Instruction ~> Id) {
      var step = 0

      def apply[A](a: Instruction[A]) = a match {
        case Get => step
        case Increment =>
          step += 1
          step
      }
    }
  }

  object Observable {
    sealed trait Instuction[A]
    final case object GetMaxIStep extends Instuction[Option[MaxIStep]]
    final case class SetMaxIStep(step: Option[MaxIStep]) extends Instuction[Unit]
    final case object GetInfectionStep extends Instuction[Vector[Option[Long]]]
    final case class SetInfectionStep(step: Vector[Option[Long]]) extends Instuction[Unit]

    type DSL = Instuction :|: NilDSL
    val DSL = freek.DSL.Make[DSL]

    def interpreter = new (Instuction ~> Id) {
      var maxIStep: Option[MaxIStep] = None
      var infectionStep: Vector[Option[Long]] = Vector.empty

      def apply[A](a: Instuction[A]) = a match {
        case GetMaxIStep => maxIStep
        case SetMaxIStep(s) => maxIStep = s
        case GetInfectionStep => infectionStep
        case SetInfectionStep(v) => infectionStep = v
      }
    }
  }

  object ModelState {
    sealed trait Instruction[A]
    final case object Get extends Instruction[MicMacState]
    final case class Set(s: MicMacState) extends Instruction[Unit]

    type DSL = Instruction :|: NilDSL
    val DSL = freek.DSL.Make[DSL]

    def interpreter = new (Instruction ~> Id) {
      var state: Option[MicMacState] = None

      //FIXME use onion
      def apply[A](a: Instruction[A]) = a match {
        case Get => state.getOrElse(throw new RuntimeException("state as not been set"))
        case Set(v) => state = Some(v)
      }
    }
  }

  type DSL =
    Step.DSL :||:
    Observable.DSL :||:
    ModelState.DSL :||:
    freedsl.random.DSL :||:
    freedsl.log.DSL

  val DSL = freek.DSL.Make[DSL]

  type Context[T] = Free[DSL.Cop, T]

  def interpreter(seed: Long) =
    Step.interpreter :&:
      Observable.interpreter :&:
      freedsl.random.interpreter(seed) :&:
      ModelState.interpreter :&:
      freedsl.log.interpreter

  implicit def random = freedsl.random.impl[DSL]
  implicit def log = freedsl.log.impl[DSL]

  implicit def step = new Step[Context] {
    override def get = Step.Get.freek[DSL]
    override def increment = Step.Increment.freek[DSL]
  }

  implicit def modelState = new ModelState[Context] {
    def get = ModelState.Get.freek[DSL]
    def set(s: MicMacState) = ModelState.Set(s).freek[DSL]
  }

  implicit def observable = new Observable[Context] {
    def getMaxIStep = Observable.GetMaxIStep.freek[DSL]
    def setMaxIStep(s: Option[MaxIStep]) = Observable.SetMaxIStep(s).freek[DSL]
    def getInfectionStep = Observable.GetInfectionStep.freek[DSL]
    def setInfectionStep(v: Vector[Option[Long]]) = Observable.SetInfectionStep(v).freek[DSL]
  }



}
