test_that("subset_mixture_matrix returns a matrix with rownames", {

  rows <- rownames(thunder_feature_selection("test_data.txt.gz",
                            n_cell_types = 2,
                            itter = 1))

  expect_type(rows, "character")
})

test_that("subset_mix_out_path argument of thunder_feature_selection saves bin_names", {

            thunder_feature_selection("test_data.txt.gz",
                                      n_cell_types = 2,
                                      itter = 1,
                                      subset_mix_out_path = "test_subset.txt.gz")

            subset <- read_tsv("test_subset.txt.gz")



            expect_true("bin_name" %in% colnames(subset))
            expect_type(subset$bin_name, "character")

          })
