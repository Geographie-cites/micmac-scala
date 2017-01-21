package micmac

import freedsl.tool._
import context._
import cats._
import cats.implicits._
import dynamic._
import monocle.function._
import observable._
import integrator._
import monocle.function._
import monocle.std.all._
import freedsl.random.Random

object simulation {

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
      _ <- updatePlaneSIR
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
    } yield s
  }
}
