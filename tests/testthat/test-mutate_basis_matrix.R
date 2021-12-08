test_that("Basis matrix colnames are formatted correctly", {


  k <- 2

  #Process input data.
  .raw_mix <- read_tsv("new_cols_test_data2.tsv",
                       show_col_types = FALSE) %>%
    filter(rowSums(across(-c(1,2))) > 0)

  .feature_name <- .raw_mix$feature_name
  .contact_type <- .raw_mix$contact_type

  .mix <- .raw_mix %>%
    select(-c(1,2))

  #NMF fit
  test_fit <- nmf_fit(mixture = .mix, n_cell_types = k, itter = 1)

  #We want to check that the first k columns are formatted as we expect.
  cols <- colnames(mutate_basis_matrix(test_fit, feature_name = .feature_name, contact_type = .contact_type))

  print(cols)

  expect_true(sum(str_detect(cols, "celltype[0-9]*_features")) == k)
  expect_true(any(str_detect(cols, "contact_type")))
  expect_true(any(str_detect(cols, "feature_name")))

})

# test_that("std deviation works with more than two cell types", {
#
#   test_fit <- run_thunder("new_cols_test_data2.tsv", n_cell_types = 5,
#                       itter = 1,
#                       subset_mix_out_path = "test_subset_out.txt.gz")
#
#   sd_vec <- mutate_basis_matrix(test_fit) %>%
#     pull(std_dev)
#
#   expect_true(is.numeric(sd_vec))
#   expect_true(all(sd_vec > 0))
#
# })
#
# test_that("feature score works with more than two cell types", {
#
#   test_fit <- run_thunder("new_cols_test_data2.tsv", n_cell_types = 5,
#                       itter = 1,
#                       subset_mix_out_path = "test_subset_out.txt.gz")
#
#   fs_vec <- mutate_basis_matrix(test_fit) %>%
#     pull(feature_score)
#
#   expect_true(is.numeric(fs_vec))
#
# })
#
