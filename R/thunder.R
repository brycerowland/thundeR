#'
#'
#'@importFrom readr read_tsv
#'@importFrom tibble column_to_rownames
#'@importFrom dplyr filter
#'@importFrom dplyr %>%
#'

#' @export
#'

thunder <- function(path_to_mixture, n_cell_types, itter,
                    out_init_nmf = NULL){
  .mix <- read_tsv(path_to_mixture) %>%
    column_to_rownames("bin_name") %>%
    filter(rowSums(.) != 0)


  .fit_init <- initial_nmf_fit(mixture = .mix,
                              n_cell_types = n_cell_types,
                              itter = itter)
  if ( is.character(out_init_nmf) ){
    print("Saving initial NMF fit . . .")
    saveRDS(object = .fit_init, file = out_init_nmf)
  }
  return(.fit_init)
}
