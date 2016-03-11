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

import scalaz._
import Scalaz._

object network {

  def randomAirports[M[_]: Monad: RNG](
    territory: Territory,
    buildAirport: (Int, Double, Double) => Airport,
    number: Int) = {

    def randomAirport(i: Int): M[Airport] =
      for {
        rng <- implicitly[RNG[M]].rng
      } yield {
        val x = (rng.nextDouble * territory.length)
        val y = (rng.nextDouble * territory.width)
        buildAirport(i, x, y)
      }

    (0 until number).toVector.traverseU(randomAirport)
  }

  def randomNetwork[M[_]: Monad](edges: Int)(implicit mRNG: RNG[M]) = Kleisli[M, Vector[Airport], Network] {  airports: Vector[Airport] =>
    for {
      rng <- implicitly[RNG[M]].rng
    } yield {
      def randomEdge = (rng.nextInt(airports.size), rng.nextInt(airports.size))

      def randomNetwork(size: Int, links: Set[(Int, Int)]): Network =
        if(size == 0) Network(airports, links.toVector)
        else {
          val candidate = randomEdge
          if(!links.contains(candidate)) randomNetwork(size - 1, links + candidate)
          else randomNetwork(size, links)
        }

      randomNetwork(edges, Set.empty)
    }
  }


}
