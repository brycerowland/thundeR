#' Compute summary statistics on rows of cell type profile matrix
#'
#' @importFrom NMF basis
#' @importFrom tibble as_tibble
#' @import dplyr
#' @importFrom NMF featureScore
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom tidyr unnest
#'
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
    mutate(feature_score = featureScore(nmf_obj),
           contact_type = if_else(
             str_split(bin_pair[[1]], "_")[[1]][2] == str_split(bin_pair[[1]], "_")[[1]][5],
             "intra", "inter"
           )) %>%
    return()
}

#' Subset Initial NMF fit after Feature Selection
#'
#' @importFrom NMF basis
#' @importFrom tibble as_tibble
#' @import dplyr
#' @importFrom NMF featureScore
#' @importFrom tibble rownames_to_column
#' @importFrom stringr str_detect
#' @importFrom stringr str_split
#' @importFrom tidyr unnest
#'
#' @param mixture_data mixture data from Hi-C experiment.
#' @param nmf_obj NMF object - usually the output of `initial_nmf_fit`
#'
#'
#' @export
subset_init_nmf <- function(mixture_data, nmf_obj){
  .basis_m <- mutate_basis_matrix(nmf_obj)

  rows <- .basis_m %>%
    group_by(contact_type) %>%
    summarise(thunder_bins = list(which(
      (.$feature_score > mean(.$feature_score) + 3*sd(.$feature_score)) |
        (.$std_dev > mean(.$std_dev) + 3*sd(.$std_dev))
    ))) %>%
    tidyr::unnest(thunder_bins) %>%
    ungroup() %>%
    arrange(thunder_bins) %>%
    pull(thunder_bins)
  subset_mix <- mixture_data[rows,]
  return(subset_mix)
}





