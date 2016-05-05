package expedia.model.userdestsim

/**
 * @param clusterByDistMap Map[(userId, destId),[all clusters for the key]]
 */
case class UserDestSimPredictionModel(clustersByUserDestMap: Map[Tuple2[Double, Double], List[Double]]) {
  
}