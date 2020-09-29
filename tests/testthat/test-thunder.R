test_that("out_init_nmf option outputs a .rds file", {

  thunder("example_data/test_data.txt.gz", n_cell_types = 2,
          itter = 1,
          out_init_nmf = "example_data/test_data_init_out.rds")

  expect_true(file.exists("example_data/test_data_init_out.rds"))
  file.remove("example_data/test_data_init_out.rds")
})

