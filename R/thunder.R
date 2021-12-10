#'  Step One of THUNDER algorithm.
#'
#'@importFrom readr read_tsv
#'@importFrom readr write_tsv
#'@importFrom tibble column_to_rownames
#'@importFrom tibble rownames_to_column
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
thunder_feature_selection <- function(.raw_mix,
                                      subset_mix_out_path = NULL,
                                      n_cell_types, itter=200,
                    out_init_nmf = NULL,
                    .run_step_two){

  .feature_name <- .raw_mix$feature_name
  .contact_type <- .raw_mix$contact_type

  .mix <- .raw_mix %>%
    select(-c(1,2))

  .fit_init <- nmf_fit(mixture = .mix,
                              n_cell_types = n_cell_types,
                              itter = itter)
  if ( is.character(out_init_nmf) ){
    cat("\nSaving initial NMF fit . . .\n")
    saveRDS(object = list(.fit_init = .fit_init,
                          .feature_name = .feature_name,
                          .contact_type = .contact_type),
            file = out_init_nmf)
  }

  .subset_mix <- subset_init_nmf(.mix, .fit_init,
                                 feature_name = .feature_name,
                                 contact_type = .contact_type)

  #If we detect no informative bins, don't run step two.
  if(nrow(.subset_mix) == 0){
    cat("\n")
    warning("THUNDER did not detect informative bin-pairs.\n")
    .run_step_two = FALSE
  }

  return_list <- list(run_step_two = .run_step_two,
                      fit_init = .fit_init)


  if(.run_step_two == TRUE){
    if (is.character(subset_mix_out_path)){
      .subset_mix %>%
        write_tsv(file = subset_mix_out_path)
      return(return_list)
    }
    else{
      return(.subset_mix)
    }
  }
  else{
    return(return_list)
  }
}

#' Step Two of THUNDER algorithm
#'
#'
#' @importFrom readr read_tsv
#' @importFrom tibble column_to_rownames
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

  .subset_mixture <- read_tsv(.subset_mixture_path,
                              show_col_types = FALSE) %>%
    select(-c("feature_name", "contact_type"))

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
                    out_init_nmf = NULL,
                    subset_mix_out_path) {

  #Process input data.
  .raw_mix <- read_tsv(path_to_mixture,
                       show_col_types = FALSE) %>%
    filter(rowSums(across(-c(1,2))) > 0)

  #Process input data.
  if(("feature_name" != colnames(.raw_mix)[[1]])){
    stop("First column must be feature_name and contain informative feature names.")
  }

  if(("contact_type" != colnames(.raw_mix)[[2]])){
    stop("Second column must be contact_type: either intra or inter.")
  }


  if(!all(unique(.raw_mix$contact_type) %in% c("intra", "inter"))){

    u_c_types <- unique(.raw_mix$contact_type)

    bad_types <- u_c_types[which(!( u_c_types %in% c("intra", "inter")))]

    bad_types_str <- paste(bad_types, collapse = ", ")

    stop(sprintf("Mixture file contains bad column type(s): %s",
                 bad_types_str))
  }

  run_step_two <- TRUE

  cat("\nRunning feature selection\n")
  #Returns a list with initial NMF fit, subset NMF fit object, and run step two boolean.
  step1_l <- thunder_feature_selection(.raw_mix, n_cell_types, itter,
                                           out_init_nmf =out_init_nmf,
                            subset_mix_out_path = subset_mix_out_path,
                            .run_step_two = run_step_two)

  run_step_two <- step1_l$run_step_two

  if(run_step_two){
    cat("\nEstimating Cell Type Proprotions with THUNDER Informative Bins\n")

    .subset_fit <- thunder_estimate_CTP(.subset_mixture = subset_mix_out_path,
                                        n_cell_types,
                                        itter)
    return(.subset_fit)
  } else{
    cat("\nReturning Initial NMF fit as final deconvolution estimate.\n")

    return(step1_l$fit_init)
  }



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

