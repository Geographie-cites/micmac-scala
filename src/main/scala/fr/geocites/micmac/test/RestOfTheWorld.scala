package fr.geocites.micmac.test

import fr.geocites.micmac._
import concurrent.duration._
import cats._
import cats.implicits._
import stop._
import freedsl.log._
import freedsl.tool._

object RestOfTheWorld extends App {


  def distributions = Vector(
    Vector(1, 10, 10, 10, 10, 10),
    Vector(1, 5, 10, 10, 10, 15),
    Vector(1, 1, 5, 10, 14, 20),
    Vector(1, 1, 1, 10, 18, 20),
    Vector(1, 1, 1, 5, 15, 28),
    Vector(1, 1, 1, 1, 1, 46)
  )

  val territory = Territory(1000, 1000)
  val alpha = 0.2
  val beta = 0.5
  val sirIntegrator = integrator.integrateSIR(0.01)
  val populationByNode = 1000
  val cut = 0

  def sir(s: Double, i: Double, r: Double) =
    SIR(s = s, i = i, r = r,  alpha = alpha, beta = beta)

  def buildAirport(index: Int, x: Double, y: Double, infected: Int) =
    Airport(
      index, x, y,
      sir(s = populationByNode - infected, infected, r = 0),
      0
    )


  def networks = distributions.map { dist =>
    network.starNetwork(territory, buildAirport, dist.dropRight(cut))
  }

  val populationToFly =
    dynamic.populationToFly(
      sir = sir(s = populationByNode - 1, i = 1, r = 0),
      integrator = sirIntegrator,
      epidemyDuration = 60 days,
      epsilon = 0.001,
      mobilityRate = 0.017,
      nbAirports = 6
    )


  import context._
  import merged._

  val simulations =
    networks.map { network =>
      def initState = implicitly[ModelState[M]].set(MicMacState(network, Vector.empty))
      def evolve = simulation.step[M](sirIntegrator, sirIntegrator, 80, 10, sir(_,_,_), populationToFly)
      def end = or(endOfEpidemy[M], stopAfter[M](200000))
      def log(s: Int) = implicitly[Log[M]].print(s"Step $s")

      network ->
        (for {
          _ <- initState
          _ <- (evolve flatMap log).until(end)
          ind <- observable.indicators[M]
        } yield ind)
    }

  val seeds = (0 until 10)

  val int = simulations.map { case(network, s) =>
    val repli = seeds.map(seed => result(s, interpreter(seed))).map(_.right.get.maxIStep.get.infectedByAirport(0).value)
    Vector(tool.gini(network.airports.tail.map(_.sir.i)), (repli.sum / repli.size))
  }

  println(int.map(_.mkString(",")).mkString("\n"))

}
