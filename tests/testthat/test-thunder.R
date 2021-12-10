test_that("THUNDER gives an informative error message when feature_name is dropped", {

  #First column should be feature_name
  expect_error(run_thunder("old_format_test_data.txt",
                           n_cell_types = 2,
                           itter = 1,
                           subset_mix_out_path = "new_cols_subset.txt.gz"),
               "First column")

})

test_that("THUNDER gives an informative error messages for contact_type", {



  expect_error(run_thunder("error_in_contact_type.tsv",
                           n_cell_types = 2,
                           itter = 1,
                           subset_mix_out_path = "new_cols_subset.txt.gz"),
               "tomato, potato")

  #Second column should be contact_type
  expect_error(run_thunder("old_no_contact_type copy.txt",
                           n_cell_types = 2,
                           itter = 1,
                           subset_mix_out_path = "new_cols_subset.txt.gz"),
               "Second column")

})


test_that("out_init_nmf option outputs a .rds file", {

  run_thunder("new_cols_test_data2.tsv", n_cell_types = 2,
          itter = 1,
          out_init_nmf = "test_data_init_out.rds",
          subset_mix_out_path = "test_subset_out.txt.gz")

  expect_true(file.exists("test_data_init_out.rds"))
  file.remove("test_data_init_out.rds")
})



test_that("When Step 2 is not run init fit is returned", {

  set.seed(13)
  mix <- read_tsv("new_cols_test_data2.tsv",
                  show_col_types = F) %>%
    select(-c(1,2)) %>%
    filter(rowSums(.) > 0)

  expect_warning(run_thunder("new_cols_test_data2.tsv", n_cell_types = 2,
                             itter = 1,
                             subset_mix_out_path = "test_subset_out.txt.gz"),
                 "THUNDER did not detect")

  thunder_fit <- run_thunder("new_cols_test_data2.tsv", n_cell_types = 2,
              itter = 1,
              subset_mix_out_path = "test_subset_out.txt.gz")

  expect_true(nrow(basis(thunder_fit)) == nrow(mix))

})
