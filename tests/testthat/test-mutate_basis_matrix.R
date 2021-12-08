k <- 3

#Process input data.
.raw_mix <- read_tsv("new_cols_test_data2.tsv",
                     show_col_types = FALSE) %>%
  filter(rowSums(across(-c(1,2))) > 0)

.feature_name <- .raw_mix$feature_name
.contact_type <- .raw_mix$contact_type

.mix <- .raw_mix %>%
  select(-c(1,2))

#NMF fit
test_fit <- nmf_fit(mixture = .mix, n_cell_types = k, itter = 1)

m_basis <- mutate_basis_matrix(test_fit, feature_name = .feature_name, contact_type = .contact_type)

test_that("Basis matrix colnames are formatted correctly", {

  #We want to check that the first k columns are formatted as we expect.
  cols <- colnames(m_basis)

  expect_true(sum(str_detect(cols, "celltype[0-9]*_features")) == k)
  expect_true(any(str_detect(cols, "contact_type")))
  expect_true(any(str_detect(cols, "feature_name")))

})

test_that("std deviation works with more than two cell types", {


  sd_vec <- m_basis %>% pull(std_dev)

  expect_true(is.numeric(sd_vec))
  expect_true(all(sd_vec > 0))

})

test_that("feature score works with more than two cell types", {

  fs_vec <- m_basis %>% pull(feature_score)

  expect_true(is.numeric(fs_vec))

})

test_that("subset_mix_out_path argument of thunder_feature_selection saves feature_name and contact_type", {

  subset <- subset_init_nmf(mixture_data = .mix, nmf_obj = test_fit,
                             feature_name = .feature_name,
                             contact_type = .contact_type)



  expect_true("feature_name" %in% colnames(subset))
  expect_true("contact_type" %in% colnames(subset))

})




