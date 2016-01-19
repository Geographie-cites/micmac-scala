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
package fr.geocites

import monocle.macros.Lenses

import scala.concurrent.duration.Duration
import scala.util.Random
import simulacrum._


package object micmac {

  @typeclass trait RNG[M[_]] {
    def rng: M[Random]
  }

  case class Territory(length: Int, width: Int)
  @Lenses case class SIR(s: Double, i: Double, r: Double, alpha: Double, beta: Double) {
    def normalisedBeta =  beta / (s + i + r)
  }
  @Lenses case class Network(nodes: Vector[Airport], edges: Vector[(Int, Int)])
  @Lenses case class Airport(i: Int, x: Double, y: Double, sir: SIR)
  //@Lenses case class World(network: Network)

  case class Plane(capacity: Int, sir: SIR)
  case class Flight(plain: Plane, destination: Airport)

}
