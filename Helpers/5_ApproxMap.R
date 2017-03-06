#' get_consensus_pattern
#'
#' Gets the consensus pattern from the weighted sequence
#'
#' @param weighted_seq A weighted sequence
#' @param strength The cutoff to extract the consensus pattern from the weighted sequence
#' @export
#'
#' @return Returns the consensus pattern if you give the weighted sequence
#'
#' @examples get_consensus_pattern(weighted_seq, 0.4)


get_consensus_pattern = function(weighted_seq, strength) {

  n = weighted_seq$n
  weighted_seq$n = NULL

  min_occurences = n * strength
  consensus_pattern = list()

  for(i in 1:length(weighted_seq)) {
    itemset = weighted_seq[[i]]
    strength_test = itemset$element_weights > min_occurences
    elements = (itemset$elements[strength_test])
    #consensus_pattern = append(consensus_pattern,elements,i-1)
    if(length(elements)>0) consensus_pattern[[length(consensus_pattern)+1]] = elements
  }

  return(consensus_pattern)
}



#' get_approxMap
#'
#' Does al the steps in approxmap algorithm
#'
#' @param seqList A list of sequences
#' @param k The number of nearest neighbours to look at
#' @param strength The cutoff to extract the consensus pattern from the weighted sequence
#' @export
#'
#' @return Clusters and gives the consensus pattern from the sequence list
#'
#' @examples get_approxMap(sequences_list, 2, 0.4)

get_approxMap = function(seqList,k,strength, id = 1) {

  if(id==1) id = 1:length(seqList)

  cluster_info = knnCluster(seqList,k,id)
  clusters = cluster_info$Cluster
  consensus_patterns = list()
  cluster_ids = list()
  weighted_seqs = list()

  for(i in 1:length(unique(clusters))) {
    current_cluster = clusters == unique(clusters)[i]
    cluster_ids[[i]] = id[current_cluster]
    current_density = cluster_info$Density[current_cluster]
    current_seqs = seqList[current_cluster]
    current_seqs = current_seqs[order(-current_density)]
    weighted_seqs[[i]] = align_multiple_sequences(current_seqs)
    consensus_patterns[[i]] = get_consensus_pattern(weighted_seqs[[i]],strength)
    #cluster_pattern = list(ID = current_id, consensus_pattern = consensus_pattern,weighted_alignment = weighted_alignment)
    #consensus_patterns[[i]] = cluster_pattern
  }

  formatted_con_pat = lapply(consensus_patterns, get_consensus_formatted)
  formatted_weighted_seqs = lapply(weighted_seqs, get_Wseq_Formatted)
  formatted_results = list(weighted_seq = formatted_weighted_seqs, consensus = formatted_con_pat)
  results = list(clusters = cluster_ids, weighted_seqs = weighted_seqs, consensus_patterns = consensus_patterns, formatted_results = formatted_results)
  return(results)
}
