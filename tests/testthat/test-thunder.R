library(NMF)

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

  set.seed(1)
  run_thunder("new_cols_test_data2.tsv", n_cell_types = 2,
          itter = 1,
          out_init_nmf = "test_data_init_out.rds",
          subset_mix_out_path = "test_subset_out.txt.gz")

  expect_true(file.exists("test_data_init_out.rds"))
  file.remove("test_data_init_out.rds")
})



test_that("When Step 2 is not run init fit is returned", {

  junk_matrix <- tibble(
    feature_name = paste0("potato", "1"),
    contact_type = "intra",
    X1 = rep(1, 1),
    X2 = rep(1, 1),
    X3 = rep(1, 1)
  )

  expect_warning(
    thunder_feature_selection(.raw_mix = junk_matrix,
                              subset_mix_out_path = "test",
                              n_cell_types = 2, itter = 1,
                              out_init_nmf = "out.rds",
                              .run_step_two = F), regexp = "did not detect")

  l <- thunder_feature_selection(.raw_mix = junk_matrix,
                            subset_mix_out_path = "test",
                            n_cell_types = 2, itter = 1,
                            out_init_nmf = "out.rds")

  expect_false(l$run_step_two)

})
