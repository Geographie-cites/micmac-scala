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

import org.apache.commons.math3.ode.FirstOrderDifferentialEquations
import org.apache.commons.math3.ode.nonstiff.{ClassicalRungeKuttaIntegrator, RungeKuttaIntegrator}

object sir {

  type Integrator = SIR => SIR

  def integrator(t: Double, h: Double): Integrator = (sir: SIR) => {
    val equations = new FirstOrderDifferentialEquations {
      override def getDimension: Int = 3
      override def computeDerivatives(t: Double, y: Array[Double], yDot: Array[Double]): Unit = {
        yDot(0) = (-sir.normalisedBeta * y(1) * y(0))
        yDot(1) = (sir.normalisedBeta * y(1) * y(0)) - (sir.alpha * y(1))
        yDot(2) = (sir.alpha * y(1))
      }
    }

    val integrator = new ClassicalRungeKuttaIntegrator(h)

    val y = Array.ofDim[Double](3)
    integrator.integrate(equations, 0, Array(sir.s, sir.i, sir.r), t, y)

    sir.copy(s = y(0), i = y(1), r = y(2))
  }


}
