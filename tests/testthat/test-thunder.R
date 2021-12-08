test_that("out_init_nmf option outputs a .rds file", {

  run_thunder("test_data.txt.gz", n_cell_types = 2,
          itter = 1,
          out_init_nmf = "test_data_init_out.rds",
          subset_mix_out_path = "test_subset_out.txt.gz")

  expect_true(file.exists("test_data_init_out.rds"))
  file.remove("test_data_init_out.rds")
})



test_that("When Step 2 is not run init fit is returned", {

  set.seed(13)
  mix <- read_tsv("test_data.txt.gz",
                  show_col_types = F) %>%
    column_to_rownames("bin_name") %>%
    filter(rowSums(.) != 0)


  thunder_fit <- run_thunder("test_data.txt.gz", n_cell_types = 2,
              itter = 1,
              out_init_nmf = "test_data_init_out.rds",
              subset_mix_out_path = "test_subset_out.txt.gz")



  expect_true(nrow(basis(thunder_fit)) == nrow(mix))
})
