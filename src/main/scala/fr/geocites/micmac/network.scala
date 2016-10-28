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


import cats._
import cats.implicits._
import cats.data._
import monocle.std.all._
import freedsl.random._

object network {

  def randomAirports[M[_]: Monad](
    territory: Territory,
    buildAirport: (Int, Double, Double, Int) => Airport,
    number: Int)(implicit rng: Random[M]) = {

    def randomAirport(i: Int, infectedIndex: Int) =
      for {
        x <- rng.nextDouble.map(_ * territory.length)
        y <- rng.nextDouble.map(_ * territory.width)
      } yield {
        def infected = if(infectedIndex == i) 1 else 0
        buildAirport(i, x, y, infected)
      }

    for {
      infectedIndex <- rng.nextInt(number)
      airports <- (0 until number).toVector.traverseU(randomAirport(_, infectedIndex))
    } yield airports
  }

  def randomNetwork[M[_]: Monad](edges: Int, airports: Vector[Airport])(implicit rng: Random[M], modelState: ModelState[M]) = {

    def fullSet =
      for {
        i <- 0 until airports.size
        j <- 0 until airports.size
        if i != j
      } yield (i, j)

    def fullLinkCount = Vector.fill(airports.size)(airports.size - 1)


    case class RandomNetworkParam(links: Set[(Int, Int)], numberOfIncomingLinks: Vector[Int], numberOfOutgoingLinks: Vector[Int])

    Monad[M].tailRecM(RandomNetworkParam(fullSet.toSet, fullLinkCount, fullLinkCount)) { param =>
      def randomEdge(links: Set[(Int, Int)]) =
        for { i <- rng.nextInt(links.size) } yield links.toVector(i)

      if (param.links.size == edges) (Right(Network(airports, param.links.toVector)): Either[RandomNetworkParam, Network]).pure[M]
      else
        randomEdge(param.links).map { candidate =>
          import param._

          def isValid = numberOfOutgoingLinks(candidate._1) > 1 && numberOfIncomingLinks(candidate._2) > 1

          val newParams =
            if (!isValid) RandomNetworkParam(links, numberOfIncomingLinks, numberOfIncomingLinks)
            else
              RandomNetworkParam(
                links - candidate,
                vectorIndex[Int].index(candidate._1).modify(_ - 1) apply (numberOfOutgoingLinks),
                vectorIndex[Int].index(candidate._2).modify(_ - 1) apply (numberOfIncomingLinks))

          (Left(newParams): Either[RandomNetworkParam, Network])
        }
    }

  }

}

