#' Initial NMF fit for mixture matrix of Hi-C data
#'
#' @importFrom NMF nmf
#'
#' @param mixture Input mixture matrix for deconvolution where rows are Hi-C features and columns are samples.
#' @param out Complete output file path including name of output .rds
#' @param cell_types Integer. The number of columns in the basis matrix of the deconvolution. Corresponds to the number of cell types in bulk Hi-C mixture.
#'
#' @export

initial_nmf_fit <- function(mixture, out, cell_types = 2){
  fit <- nmf(mixture, rank = cell_types, method="brunet",seed="random",nrun=200)
  saveRDS(fit, file = out)
}
