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

import fr.geocites.micmac.sir.Integrator

import scalaz._
import Scalaz._

object dynamic {

  def evolve[T](integrator: Integrator, sir: monocle.Traversal[T, SIR])(t: T) =
    sir.modify(integrator)(t)

  def updateStep[M[_]: Monad: Step, T] = Kleisli[M, T, T] { t: T => implicitly[Step[M]].modify(_ + 1).map(_ => t) }
  def stopAfter[M[_]: Monad: Step](s: Long) = implicitly[Step[M]].get.map { _ >= s }

}
