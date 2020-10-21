#'  Step One of THUNDER algorithm.
#'
#'@importFrom readr read_tsv
#'@importFrom readr write_tsv
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
thunder_feature_selection <- function(path_to_mixture,
                                      subset_mix_out_path = NULL,
                                      n_cell_types, itter=200,
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

  if (is.character(subset_mix_out_path)){
    print("Saving subset mixture data")
    write_tsv(.subset_mix,
              path = subset_mix_out_path)
  }
  else{
    return(.subset_mix)
  }
}

#' Step Two of THUNDER algorithm
#'
#'
#' @importFrom readr read_tsv
#'
#' @param .subset_mixture Matrix. Usually the output of `thunder_feature_selection`
#' @param n_cell_types Integer. The number of columns in the basis matrix of the deconvolution. Corresponds to the number of cell types in bulk Hi-C mixture.
#' @param itter Integer. Number of itterations for NMF algorithm. Default is 200.
#'
#' @export
#'
thunder_estimate_CTP <- function(.subset_mixture_path,
                                 n_cell_types,
                                 itter){

  .subset_mixture <- read_tsv(.subset_mixture_path)

  .subset_fit <- nmf_fit(mixture = .subset_mixture,
                         n_cell_types = n_cell_types,
                         itter = itter)
  return(.subset_fit)

}


#' THUNDER estimates cell type proportions in bulk Hi-C datasets
#'
#' @param path_to_mixture String. Path to .tsv file of bulk Hi-C samples.
#' @param n_cell_types Integer. The number of columns in the basis matrix of the deconvolution. Corresponds to the number of cell types in bulk Hi-C mixture.
#' @param itter Integer. Number of itterations for NMF algorithm. Default is 200.
#' @param out_init_nmf String. Default is NULL. If character, then saves the initial NMF fit as a .RDS object to specified file path.
#'
#' @export
#'
run_thunder <- function(path_to_mixture, n_cell_types, itter=200,
                    out_init_nmf = NULL) {
  .subset_mix <- thunder_feature_selection(path_to_mixture, n_cell_types, itter,
                                           out_init_nmf =out_init_nmf)
  .subset_fit <- thunder_estimate_CTP(.subset_mixture = .subset_mix,
                       n_cell_types,
                       itter)
  return(.subset_fit)
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

