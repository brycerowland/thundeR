mutate_basis_matrix <- function(nmf_obj, type, name){
  .basis <- basis(nmf_obj)

  colnames(.basis) <- str_c("celltype", seq(1:dim(.basis)[[2]]), "_features")

  .basis %>%
    as_tibble() %>%
    mutate(featureScore(nmf_obj))
}
