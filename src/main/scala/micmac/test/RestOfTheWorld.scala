package micmac.test

import micmac._
import squants.time.TimeConversions._
import cats._
import cats.implicits._
import micmac.context.{ModelState, Observable, Step}
import micmac.dynamic.BuildSIR
import micmac.integrator.Integrator
import monocle.function.Each
import stop._
import monocle.function._
import monocle.std.all._

import freedsl.random._
import freedsl.tool._
import freedsl.log._

object RestOfTheWorld extends App {

  import dynamic._
  import observable._

  def step[M[_]: Monad: ModelState: Random: Step: Observable](
   airportIntegrator: Integrator,
   planeIntegrator: Integrator,
   planeCapacity: Int,
   planeSpeed: Double,
   planeSIR: BuildSIR,
   populationToFly: Double) = {

    def stateModifier = {
      def modelState = implicitly[ModelState[M]]
      modifier(modelState.get, modelState.set)
    }

    def updateAirportSIR =
      stateModifier apply {
        dynamic.updateSIRs(
          airportIntegrator,
          MicMacState.network composeTraversal Network.airportsTraversal composeLens Airport.sir
        )
      }

    def updatePlaneSIR =
      stateModifier apply {
        dynamic.updateSIRs(
          planeIntegrator,
          MicMacState.flyingPlanes composeTraversal Each.each composeLens Plane.sir
        )
      }

    for {
      _ <- updateAirportSIR
      //_ <- updatePlaneSIR
      _ <-
        planeDepartures[M](
          planeCapacity = planeCapacity,
          populationToFly = populationToFly,
          destination = dynamic.randomDestination[M],
          buildSIR = planeSIR)
      _ <- planeArrivals[M](planeSpeed)
      _ <- updateMaxStepI[M]
      s <- updateStep[M]
      _ <- updateInfectedNodes[M]
      pop <- implicitly[ModelState[M]].get.map(_.network.airports.map(_.sir.total))
      _ = println(pop)
    } yield s
  }


//  def distributions = Vector(
//    Vector(1, 10, 10, 10, 10, 10),
//    Vector(1, 5, 10, 10, 10, 15),
//    Vector(1, 1, 5, 10, 14, 20),
//    Vector(1, 1, 1, 10, 18, 20),
//    Vector(1, 1, 1, 5, 15, 28),
//    Vector(1, 1, 1, 1, 1, 46)
//  )

  def distributions = Vector(Vector(1, 1, 1))

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
      nbAirports = 2
    )


  def simulations[M[_]: Monad: ModelState: Random: Step: Observable: Log] =
    networks.map { network =>
      def initState = implicitly[ModelState[M]].set(MicMacState(network, Vector.empty))
      def evolve = step[M](sirIntegrator, sirIntegrator, 80, 10, sir(_,_,_), populationToFly)
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

  import micmac.context._
  import merged.implicits._

  val int = simulations[merged.M].map { case(network, s) =>
    val repli = seeds.map(seed => interpreter(seed).run(s)).map(_.right.get.maxIStep.get.infectedByAirport(0).value)
    Vector(tool.gini(network.airports.tail.map(_.sir.i)), (repli.sum / repli.size))
  }

  println(int.map(_.mkString(",")).mkString("\n"))

}
