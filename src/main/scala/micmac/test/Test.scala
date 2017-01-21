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
package micmac.test

import micmac._
import network._
import context._
import dynamic._
import integrator._
import observable._
import stop._
import freedsl.log._
import freedsl.tool._
import cats._
import cats.implicits._
import monocle.function._
import monocle.std.all._
import squants.time.TimeConversions._

object Test extends App {

  val territory = Territory(1000, 1000)
  val nodes = 50
  val edges = 500
  val populationByNode = 4000

  val alpha = 0.2
  val beta = 0.5
  val planeCapacity = 80
  val planeSpeed = 0.5

  val sirIntegrator = integrator.integrateSIR(0.01)

  def sir(s: Double, i: Double, r: Double) =
    SIR(s = s, i = i, r = r,  alpha = alpha, beta = beta)

  val populationToFly =
    dynamic.populationToFly(
      sir = sir(s = populationByNode - 1, i = 1, r = 0),
      integrator = sirIntegrator,
      epidemyDuration = 60 days,
      epsilon = 0.001,
      mobilityRate = 0.017,
      nbAirports = nodes
    )

  import merged._
  import merged.implicits._

  def airports =
    randomAirports[M](
      territory,
      (index, x, y, infected) =>
        Airport(
          index, x, y,
          sir(s = populationByNode - infected, infected, r = 0),
          populationToFly
        ),
      nodes
    )

  def initState = for {
    a <- airports
    network <- randomNetwork[M](edges, a)
    _ <- implicitly[ModelState[M]].set(MicMacState(network, Vector.empty))
  } yield ()


  def evolve = simulation.step[M](sirIntegrator, sirIntegrator, planeCapacity, planeSpeed, sir(_,_,_), populationToFly)
  def end = or(endOfEpidemy[M], stopAfter[M](200000))
  def log(s: Int) = implicitly[Log[M]].print(s"Step $s")

  def s =
    for {
      _ <- initState
      _ <- (evolve flatMap log).until(end)
      ind <- observable.indicators[M]
    } yield ind

  println(interpreter(420).run(s))

}
