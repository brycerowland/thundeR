thunder_feature_selection("out_init_nmf option outputs a .rds file", {

  thunder("test_data.txt.gz", n_cell_types = 2,
          itter = 1,
          out_init_nmf = "test_data_init_out.rds")

  expect_true(file.exists("test_data_init_out.rds"))
  file.remove("test_data_init_out.rds")
})

