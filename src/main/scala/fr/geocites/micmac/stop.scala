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

import scalaz._
import Scalaz._

import observable._

object stop {

  type StopCondition[M[_]] = Kleisli[M, MicMacState, Boolean]

  def endOfEpidemy[M[_]: Monad](implicit observable: Observable[M], step: Step[M]): StopCondition[M] = Kleisli { state =>
    def finished(maxIStep: Option[MaxIStep], step: Long) =
      maxIStep match {
        case Some(maxIStep) =>
          if(step >= maxIStep.step + 1) total(_.i)(state) < 1.0
          else false
        case None => false
      }

    for {
      s <- step.step.get
      maxIStep <- observable.maxIStep.get
    } yield finished(maxIStep, s)
  }

  def stopAfter[M[_]: Monad](s: Long)(implicit step: Step[M]): StopCondition[M] = Kleisli { state =>
    step.step.get.map { _ >= s }
  }

  def or[M[_]: Monad](s1:  StopCondition[M], s2: StopCondition[M]) =
    for {
      c1 <- s1
      c2 <- s2
    } yield c1 || c2

}
