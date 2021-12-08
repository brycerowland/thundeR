test_that("Basis matrix colnames are formatted correctly", {

  #Generate some random number of cell types
  k <- 2
  test_fit <- run_thunder("new_cols_test_data2.tsv", n_cell_types = k,
          itter = 1,
          subset_mix_out_path = "test_subset_out.txt.gz")

  #We want to check that the first k columns are formatted as we expect.
  check_cols <- mutate_basis_matrix(test_fit) %>%
    select(all_of(2:(k+1))) %>%
    colnames()


  expect_true(all(str_detect(check_cols, "celltype[0-9]*_features")))
})

test_that("std deviation works with more than two cell types", {

  test_fit <- run_thunder("new_cols_test_data2.tsv", n_cell_types = 5,
                      itter = 1,
                      subset_mix_out_path = "test_subset_out.txt.gz")

  sd_vec <- mutate_basis_matrix(test_fit) %>%
    pull(std_dev)

  expect_true(is.numeric(sd_vec))
  expect_true(all(sd_vec > 0))

})

test_that("feature score works with more than two cell types", {

  test_fit <- run_thunder("new_cols_test_data2.tsv", n_cell_types = 5,
                      itter = 1,
                      subset_mix_out_path = "test_subset_out.txt.gz")

  fs_vec <- mutate_basis_matrix(test_fit) %>%
    pull(feature_score)

  expect_true(is.numeric(fs_vec))

})

