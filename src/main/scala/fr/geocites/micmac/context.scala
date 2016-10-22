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
import cats.implicits._

import scala.util.Random


object context {

  object Step {
    sealed trait DSL[A]
    final case object Get extends DSL[Int]
    final case object Increment extends DSL[Unit]

    type PRG = DSL :|: NilDSL
    val PRG = DSL.Make[PRG]

    def interpreter = new (DSL ~> Id) {
      var step = 0

      def apply[A](a: DSL[A]) = a match {
        case Get => step
        case Increment => step += 1
      }
    }
  }

  object Observable {
    sealed trait DSL[A]
    final case object GetMaxIStep extends DSL[Option[MaxIStep]]
    final case class SetMaxIStep(step: Option[MaxIStep]) extends DSL[Unit]
    final case object GetInfectionStep extends DSL[Vector[Option[Long]]]
    final case class SetInfectionStep(step: Vector[Option[Long]]) extends DSL[Unit]

    type PRG = DSL :|: NilDSL
    val PRG = DSL.Make[PRG]

    def interpreter = new (DSL ~> Id) {
      var maxIStep: Option[MaxIStep] = None
      var infectionStep: Vector[Option[Long]] = Vector.empty

      def apply[A](a: DSL[A]) = a match {
        case GetMaxIStep => maxIStep
        case SetMaxIStep(s) => maxIStep = s
        case GetInfectionStep => infectionStep
        case SetInfectionStep(v) => infectionStep = v
      }
    }
  }

  object RNG {
    sealed trait DSL[A]
    final case object NextDouble extends DSL[Double]
    final case class NextInt(n: Int) extends DSL[Int]
    //final case object Random extends DSL[Random]

    type PRG = DSL :|: NilDSL
    val PRG = DSL.Make[PRG]

    def interpreter(seed: Long) = new (DSL ~> Id) {
      var random = new util.Random(seed)

      def apply[A](a: DSL[A]) = a match {
        case NextDouble => random.nextDouble
        case NextInt(n) => random.nextInt(n)
      }
    }
  }

  object ModelState {
    sealed trait DSL[A]
    final case object Get extends DSL[MicMacState]
    final case class Set(s: MicMacState) extends DSL[Unit]

    type PRG = DSL :|: NilDSL
    val PRG = DSL.Make[PRG]

    def interpreter = new (DSL ~> Id) {
      var state: Option[MicMacState] = None

      //FIXME use onion
      def apply[A](a: DSL[A]) = a match {
        case Get => state.getOrElse(throw new RuntimeException("state as not been set"))
        case Set(v) => state = Some(v)
      }
    }
  }

  type PRG = Step.PRG :||: Observable.PRG :||: RNG.PRG :||: ModelState.PRG
  val PRG = DSL.Make[PRG]
  type Context[T] = Free[PRG.Cop, T]
  def interpreter(seed: Long) =
    Step.interpreter :&: Observable.interpreter :&: RNG.interpreter(seed) :&: ModelState.interpreter

  implicit def rng = new RNG[Context] {
    override def nextDouble = RNG.NextDouble.freek[PRG]
    override def nextInt(n: Int) = RNG.NextInt(n).freek[PRG]
  }

  implicit def step = new Step[Context] {
    override def get = Step.Get.freek[PRG]
    override def increment = Step.Increment.freek[PRG]
  }

  implicit def modelState = new ModelState[Context] {
    def get = ModelState.Get.freek[PRG]
    def set(s: MicMacState) = ModelState.Set(s).freek[PRG]
  }

  implicit def observable = new Observable[Context] {
    def getMaxIStep = Observable.GetMaxIStep.freek[PRG]
    def setMaxIStep(s: Option[MaxIStep]) = Observable.SetMaxIStep(s).freek[PRG]
    def getInfectionStep = Observable.GetInfectionStep.freek[PRG]
    def setInfectionStep(v: Vector[Option[Long]]) = Observable.SetInfectionStep(v).freek[PRG]
  }

}
