object BasicStats {

  def stats(values: Seq[Double]): (Double, Double) = {
    val mean = values.sum / values.length
    val std = Math.sqrt(values.foldLeft(0.0)((agg, v) =>
      agg + Math.pow(v - mean, 2)) / (values.length - 1))
    (mean, std)
  }

  def averageDist(measures: Seq[Seq[Double]]): Seq[Double] = {
    measures.map(items => stats(items)._2)
  }
}
