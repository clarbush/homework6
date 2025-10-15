#' @param n_snps The length of the sequence
#' @param n_true_seqs The number of sequences
#' @param nonzero_per_site Vector with length equal to n_snps where each entry gives sum of 1s in position
#' @return A matrix of 0's and 1's with number of rows equal to n_snps and number of columns equal to n_true_seqs
make_true_seqs = function(n_snps, n_true_seqs, nonzero_per_site) {
  true_seqs = matrix(NA, nrow = n_true_seqs, ncol = n_snps)
  
  ##check to make sure no breaks
  if(length(nonzero_per_site) != length(n_snps)){
      return ("nonzero_per_site must equal n_snps in length")
  }
  
  
  for(i in 1:n_snps) {
    snp_vals = sample(0:1, size = n_true_seqs, replace = TRUE)
    
    if(all(snp_vals == 0)) {
      snp_vals[sample(n_true_seqs, 1)] = 1
    } 
    else if(all(snp_vals == 1)) {
      snp_vals[sample(n_true_seqs, 1)] = 0
    }
    true_seqs[,i] = snp_vals
  }
  
  
  return(true_seqs)
}