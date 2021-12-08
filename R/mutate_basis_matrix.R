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
mutate_basis_matrix <- function(nmf_obj,
                                feature_name, contact_type){
  .basis <- basis(nmf_obj)

  colnames(.basis) <- paste0("celltype", seq(1:dim(.basis)[[2]]), "_features")

  #Bind columns with .feature_score and .contact_type.
  tibble(feature_name = feature_name,
         contact_type = contact_type,
         as_tibble(.basis)) %>%
    rowwise() %>%
    mutate(std_dev = sd(c_across(starts_with("celltype")))) %>%
    ungroup() %>%
    mutate(feature_score = featureScore(nmf_obj)) %>%
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
#'
#' @param mixture_data mixture data from Hi-C experiment.
#' @param nmf_obj NMF object - usually the output of `initial_nmf_fit`
#'
#'
#' @export
subset_init_nmf <- function(mixture_data, nmf_obj,
                            feature_name,
                            contact_type){

  .basis_m <- mutate_basis_matrix(nmf_obj,
                                  feature_name,
                                  contact_type)


  #Rewrite such that if no bins are selected we output message saying so and end the function.


  rows <- bind_rows(
    .basis_m %>%
      mutate(index = row_number()) %>%
      filter(contact_type ==  "inter") %>%
      filter(feature_score > mean(feature_score) + 3*sd(feature_score) |
               round(feature_score, 10) == 1 |
               std_dev > mean(std_dev) + 3 * sd(std_dev)) %>%
      summarize(thunder_bins = list(index)),
    .basis_m %>%
      mutate(index = row_number()) %>%
      filter(contact_type ==  "intra") %>%
      filter(feature_score > median(feature_score) + 3*mad(feature_score) |
               round(feature_score, 10) == 1) %>%
      summarize(thunder_bins = list(index))
  ) %>%
    tidyr::unnest(thunder_bins) %>%
    ungroup() %>%
    arrange(thunder_bins) %>%
    pull(thunder_bins)

  subset_mix <- tibble(feature_name, contact_type, mixture_data) %>% slice(rows)

  return(subset_mix)
}





