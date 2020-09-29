test_that("colnames are formatted correctly", {

  #Generate some random number of cell types
  k <- sample(2:10, 1)
  test_fit <- thunder("data/test_data.txt.gz", n_cell_types = k,
          itter = 1)

  #We want to check that the first k columns are formatted as we expect.
  check_cols <- mutate_basis_matrix(test_fit) %>%
    select(all_of(2:(k+1))) %>%
    colnames()


  expect_true(all(str_detect(check_cols, "celltype[0-9]*_features")))
})

test_that("std deviation works with more than two cell types", {

  test_fit <- thunder("data/test_data.txt.gz", n_cell_types = 5,
                      itter = 1)

  sd_vec <- mutate_basis_matrix(test_fit) %>%
    pull(std_dev)

  expect_true(is.numeric(sd_vec))
  expect_true(all(sd_vec > 0))

})

test_that("feature score works with more than two cell types", {

  test_fit <- thunder("data/test_data.txt.gz", n_cell_types = 5,
                      itter = 1)

  fs_vec <- mutate_basis_matrix(test_fit) %>%
    pull(feature_score)

  expect_true(is.numeric(fs_vec))

})

test_that("bin_pair column exists", {

  test_fit <- thunder("data/test_data.txt.gz", n_cell_types = 5,
                      itter = 1)

  bp_vec <- mutate_basis_matrix(test_fit) %>%
    pull(bin_pair)

  expect_true(is.character(bp_vec))

})

