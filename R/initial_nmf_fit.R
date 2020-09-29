#' Initial NMF fit for mixture matrix of Hi-C data
#'
#' @importFrom NMF nmf
#'
#' @param mixture Input mixture matrix for deconvolution where rows are Hi-C features and columns are samples.
#' @param n_cell_types Integer. The number of columns in the basis matrix of the deconvolution. Corresponds to the number of cell types in bulk Hi-C mixture.
#' @param itter Integer. Number of itterations for NMF algorithm. Default is 200.
#'
#' @export
initial_nmf_fit <- function(mixture, n_cell_types = 2, itter = 200){
  fit <- NMF::nmf(mixture, rank = n_cell_types, method="brunet",seed="random",nrun=itter)
  return(fit)
}
