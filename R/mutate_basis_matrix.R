mutate_basis_matrix <- function(nmf_obj, type, name){
  basis(nmf_obj) %>% as_tibble()  %>%
    rename(`celltype1_features` = V1,
           `celltype2_features` = V2) %>%
    mutate(hic_readout = str_c(name, "_", type)) %>%
    mutate(difference = celltype2_features - celltype1_features,
           fold_change = celltype2_features/celltype1_features,
           log2_fold_change = log2(fold_change),
           stdev = map2_dbl(.x = celltype1_features, .y = celltype2_features,
                            ~sd(c(.x, .y))),
           feature_score = featureScore(nmf_obj)) %>%
    return()
}
