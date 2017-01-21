package micmac

object tool {

  def gini(values: Vector[Double]) =
    if (values.length < 1) 0.0 //not computable
    else if (values.length == 1) 0.0
    else {
      val descMean = values.sum / values.size
      if (descMean == 0.0) 0.0 // only possible if all data is zero
      else {
        val vals =
          for {
            i <- 0 until values.length
            j <- 0 until values.length
            if i != j
          } yield (math.abs(values(i) - values(j)))

        val relVars = vals.sum / (2.0 * values.length * values.length)
        (relVars / descMean) // gini value
      }
    }

}
