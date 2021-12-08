test_that("subset_mix_out_path argument of thunder_feature_selection saves feature_name and contact_type", {

            thunder_feature_selection("test_data.txt.gz",
                                      n_cell_types = 2,
                                      itter = 1,
                                      subset_mix_out_path = "test_subset.txt.gz")

            subset <- read_tsv("test_subset.txt.gz",show_col_types = F)


            expect_true("bin_name" %in% colnames(subset))
            expect_true("contact_type" %in% colnames(subset))
            expect_type(subset$bin_name, "character")

          })
