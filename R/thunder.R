#' THUNDER estimates cell type proportions in bulk Hi-C datasets
#'
#'@importFrom readr read_tsv
#'@importFrom tibble column_to_rownames
#'@importFrom dplyr filter
#'@importFrom dplyr %>%
#'
#' @param path_to_mixture String. Path to .tsv file of bulk Hi-C samples.
#' @param n_cell_types Integer. The number of columns in the basis matrix of the deconvolution. Corresponds to the number of cell types in bulk Hi-C mixture.
#' @param itter Integer. Number of itterations for NMF algorithm. Default is 200.
#' @param out_init_nmf String. Default is NULL. If character, then saves the initial NMF fit as a .RDS object to specified file path.
#'
#' @export
#'
thunder <- function(path_to_mixture, n_cell_types, itter=200,
                    out_init_nmf = NULL){
  .mix <- read_tsv(path_to_mixture) %>%
    column_to_rownames("bin_name") %>%
    filter(rowSums(.) != 0)


  .fit_init <- nmf_fit(mixture = .mix,
                              n_cell_types = n_cell_types,
                              itter = itter)
  if ( is.character(out_init_nmf) ){
    print("Saving initial NMF fit . . .")
    saveRDS(object = .fit_init, file = out_init_nmf)
  }
  .subset_mix <- subset_init_nmf(.mix, .fit_init)

  .subset_fit <- nmf_fit(mixture = .subset_mix,
                         n_cell_types = n_cell_types,
                         itter = itter)
}


#' Get cell type proportions from THUNDER fit
#'
#'@importFrom NMF coef
#'
#' @param thunder_fit Output of `thunder`
#'
#' @export
#'
get_props <- function(thunder_fit){
  thunder_fit %>%
    coef()  %>%
    sweep(2, colSums(.), "/") %>%
    t()
}

