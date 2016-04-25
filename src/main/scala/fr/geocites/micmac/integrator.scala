/**
  * Created by Romain Reuillon on 25/04/16.
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

object integrator {

  def integrateSIR(h:Double)(sir: SIR): SIR = {

    def step(sir: Vector[Double], alpha: Double, beta: Double) = {
      val ds = (-beta * sir(0) * sir(1))
      val di = (beta * sir(0) * sir(1) ) - (alpha * sir(1))

      val dr = (alpha * sir(1))
      Vector(ds,di,dr)
    }

    val temp = SIR.vector.get(sir)

    val Q1 = step(temp, sir.alpha, sir.normalisedBeta)

    val sir1 = (temp zip Q1).map{
      case (t,q1) => t + (h / 2.0) * q1
    }

    val Q2 = step(sir1, sir.alpha, sir.normalisedBeta)

    val sir2 = (temp zip Q2).map {
      case (t, q2) => t + (h / 2.0) * q2
    }

    val Q3 = step(sir2, sir.alpha, sir.normalisedBeta)

    val sir3 = (temp zip Q3).map {
      case (t, q3) => t + h * q3
    }

    val Q4 = step(sir3, sir.alpha, sir.normalisedBeta)

    val newValues =
      (0 to 2).map { i => temp(i) + (h / 6.0) * (Q1(i) + 2.0 * Q2(i) + 2.0 * Q3(i) + Q4(i)) }

    SIR.vector.set(newValues.toVector)(sir)
  }


}
