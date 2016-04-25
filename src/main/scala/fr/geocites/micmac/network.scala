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
import monocle.function.all._
import monocle.std.all._

object network {

  def randomAirports[M[_]: Monad](
    territory: Territory,
    buildAirport: (Int, Double, Double, Int) => Airport,
    number: Int)(implicit rng: RNG[M]) = {

    def randomAirport(i: Int, infectedIndex: Int): M[Airport] =
      for {
        rng <- implicitly[RNG[M]].rng
      } yield {
        val x = (rng.nextDouble * territory.length)
        val y = (rng.nextDouble * territory.width)
        def infected = if(infectedIndex == i) 1 else 0
        buildAirport(i, x, y, infected)
      }

    for {
      infectedIndex <- rng.rng.map(_.nextInt(number))
      airports <- (0 until number).toVector.traverseU(randomAirport(_, infectedIndex))
    } yield airports
  }

  def randomNetwork[M[_]: Monad](edges: Int)(implicit mRNG: RNG[M]) = Kleisli[M, Vector[Airport], Network] {  airports: Vector[Airport] =>
    for {
      rng <- implicitly[RNG[M]].rng
    } yield {
      def randomEdge = (rng.nextInt(airports.size), rng.nextInt(airports.size))

      def randomNetwork(size: Int, links: Set[(Int, Int)], numberOfLinks: Vector[Int]): Network =
        if(size <= 0) Network(airports, links.toVector)
        else {
          val candidate = randomEdge
          def valid = numberOfLinks(candidate._1) > 1 && numberOfLinks(candidate._2) > 1

          if(valid) {
            def newNOL =
              vectorIndex[Int].index(candidate._1).modify(_ - 1) andThen
                vectorIndex[Int].index(candidate._2).modify(_ - 1) apply numberOfLinks

            randomNetwork(size - 1, links - candidate, newNOL)
          } else randomNetwork(size, links, numberOfLinks)
        }

      def fullSet =
        for {
          i <- 0 until airports.size
          j <- 0 until airports.size
          if i != j
        } yield (i, j)

      randomNetwork(edges, fullSet.toSet, Vector.fill(airports.size)(airports.size - 1))
    }
  }


}
