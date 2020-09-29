#' Compute summary statistics on rows of cell type profile matrix
#'
#' @importFrom NMF basis
#' @importFrom tibble as_tibble
#' @importFrom dplyr rowwise
#' @importFrom dplyr mutate
#' @importFrom dplyr ungroup
#' @importFrom dplyr c_across
#' @importFrom dplyr starts_with
#' @importFrom dplyr all_of
#' @importFrom dplyr select
#' @importFrom dplyr pull
#' @importFrom NMF featureScore
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_detect
#'
#' @param nmf_obj NMF object - usually the output of `initial_nmf_fit`
#'
#'
#' @export
mutate_basis_matrix <- function(nmf_obj){
  .basis <- basis(nmf_obj)

  colnames(.basis) <- paste0("celltype", seq(1:dim(.basis)[[2]]), "_features")

  .basis  %>%
    as_tibble(rownames = "bin_pair") %>%
    rowwise() %>%
    mutate(std_dev = sd(c_across(starts_with("celltype")))) %>%
    ungroup() %>%
    mutate(feature_score = featureScore(nmf_obj)) %>%
    return()
}
