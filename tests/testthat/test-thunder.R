test_that("out_init_nmf option outputs a .rds file", {

  run_thunder("test_data.txt.gz", n_cell_types = 2,
          itter = 1,
          out_init_nmf = "test_data_init_out.rds",
          subset_mix_out_path = "test_subset_out.txt.gz")

  expect_true(file.exists("test_data_init_out.rds"))
  # file.remove("test_data_init_out.rds")
})

