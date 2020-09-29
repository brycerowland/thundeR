test_that("colnames are formatted correctly", {

  #Generate some random number of cell types
  k <- sample(1:100, size = 1)
  test_fit <- thunder("example_data/test_data.txt.gz", n_cell_types = k,
          itter = 1)

  #We want to check that the first k columns are formatted as we expect.
  check_cols <- mutate_basis_matrix(test_fit) %>%
    select(all_of(1:k))


  any(str_detect(check_cols, "celltype[0-9]*_features"))
})

test_that("std deviation works with more than two cell types", {



})
