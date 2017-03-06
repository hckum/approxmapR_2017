#' get_weighted_sequence
#'
#' Calculates the inter-sequence distance matrix
#'
#' @param seqList A list of sequences
#' @export
#'
#' @return Returns the inter-sequence distance atrix of all the sequences
#'
#' @examples inter_seq_Distance(sequence_list)

#interStringLevenshteinDistance
inter_seq_Distance = function(seqList) {
  inter_Seq_Distance_Matrix = matrix(nrow = length(seqList),ncol = length(seqList))
  for (i in 1:nrow(inter_Seq_Distance_Matrix)) {
    for(j in 1:ncol(inter_Seq_Distance_Matrix)) {
      if(i==j) {
        inter_Seq_Distance_Matrix[i,j] = 0
      } else if(!(is.na(inter_Seq_Distance_Matrix[j,i]))) {
        inter_Seq_Distance_Matrix[i,j] = inter_Seq_Distance_Matrix[j,i]
      } else {
        dist = calculate_dist_btw_sequences(seqList[[i]],seqList[[j]])
        inter_Seq_Distance_Matrix[i,j] = dist$distance / max(dim(dist$distance_matrix))
      }
    }
  }
  #rownames(interStringDistanceMatrix) = stringList
  #colnames(interStringDistanceMatrix) = stringList
  return(inter_Seq_Distance_Matrix)
}



#' calculate_DensityInfo
#'
#' Calculates the density info of all the sequences
#'
#' @param seqList A list of sequences
#' @export
#'
#' @return Returns the density information for all the sequences
#'
#' @examples calculate_DensityInfo(sequence_list, 2)


calculate_DensityInfo = function(seqList, k) {
  results = list(length(seqList))
  inter_Seq_Distance_Matrix = inter_seq_Distance(seqList)
  for (i in 1:length(seqList)) {
    dist = inter_Seq_Distance_Matrix[i,]
    dist[i] = NA
    orderedDist = dist[order(dist)]
    k_biggestDist = orderedDist[k]
    NearestSequences = dist<=k_biggestDist

    n = sum(NearestSequences,na.rm = T)
    density = n/k_biggestDist
    item = list(density = density,NearestSequences = NearestSequences,distances = dist)
    results[[i]] = item
  }
  #names(results) = stringList
  return(results)
}



#' get_DensityArray
#'
#' Extracts the densities of all the sequences from the density info
#'
#' @param DensityInfo A list returned by calculate_DensityInfo
#' @export
#'
#' @return Returns the density information for all the sequences
#'
#' @examples get_DensityArray(Density_info)

#pass a density info object to the function and it extratcs all the densities
get_DensityArray = function(DensityInfo) {
  densityArray = numeric(length(DensityInfo))
  for (i in 1:length(DensityInfo)) {
    densityArray[i] = DensityInfo[[i]]$density
  }
  return(densityArray)
}




#' knnCluster
#'
#' Performs the knn clustering algorithm and returns clustering info
#'
#' @param seqList A list of sequences
#' @param k number of nearest neighbours to look at
#' @param id Id of every sequence in the list
#' @export
#'
#' @return Returns the id, cluster, cluster_density and the sequence_density of each sequence
#'
#' @examples knnCluster(seqList,k,id)

#clustering
knnCluster = function(seqList,k,id) {

  densityInfo = calculate_DensityInfo(seqList,k)
  densityArray = get_DensityArray(densityInfo)

  #step 1 - initialize every sequence as a cluster
  cluster = 1:length(seqList)
  clusterDensity = densityArray

  #step 2 - clustering based on criteria
  cluster_change = 1
  while(cluster_change>0) {
    cluster_change = 0
    for (i in 1:length(seqList)) {
      nSeq = densityInfo[[i]]$NearestSequences
      densityCheck = densityArray[i] < densityArray
      prelimCriteria = nSeq & densityCheck
      for (j in 1:length(seqList)) {
        if(prelimCriteria[j]) {
          if(densityInfo[[i]]$distances[j] == min(densityInfo[[i]]$distances[prelimCriteria])) {
            if(cluster[i]!=cluster[j]) {
              minCluster = min(cluster[i],cluster[j])
              cluster[cluster==cluster[i]]=minCluster
              cluster[cluster==cluster[j]]=minCluster
              maxClusterDensity = max(clusterDensity[i],clusterDensity[j])
              clusterDensity[cluster==minCluster] = maxClusterDensity
              cluster_change = cluster_change + 1
            }
          }
        }
      }
    }
  }

  #step 3 - clustering ties
  cluster_change = 1
  while(cluster_change>0) {
    cluster_change = 0
    for (i in 1:length(seqList)) {
      nSeq = densityInfo[[i]]$NearestSequences
      densityCheck = densityArray[i] < densityArray
      noNeighbourWithGreaterDensity = rep(ifelse(sum(nSeq & densityCheck)==0,T,F),length(densityArray))
      densityCheck = noNeighbourWithGreaterDensity & (densityArray[i] == densityArray)
      prelimCriteria = nSeq & densityCheck
      prelimCriteria[is.na(prelimCriteria)]=F
      for (j in 1:length(seqList)) {
        if (prelimCriteria[j]) {
          if(clusterDensity[j]>clusterDensity[i]) {
            minCluster = min(cluster[i],cluster[j])
            cluster[cluster==cluster[i]]=minCluster
            cluster[cluster==cluster[j]]=minCluster
            #maxClusterDensity = max(clusterDensity[i],clusterDensity[j])
            clusterDensity[cluster==minCluster] = clusterDensity[j]
            cluster_change = cluster_change + 1
          }
        }
      }
    }
  }

  cluster_unique = unique(cluster)
  for(cl in 1:length(cluster_unique)) {
    cluster[cluster==cluster_unique[cl]] = cl
  }

  res = data.frame("ID" = id, "Density" = round(densityArray,2), "Cluster" = cluster, "ClusterDensity" = round(clusterDensity,2))
  return(res)
}
