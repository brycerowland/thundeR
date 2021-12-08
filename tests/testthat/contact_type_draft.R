library(tidyverse)
library(NMF)
library(devtools)

build()

set.seed(13)
mix <- read_tsv("tests/testthat/test_inter_intra.txt.gz") %>%
  column_to_rownames("bin_name") %>%
  filter(rowSums(.) != 0)

fit <- nmf(mix, rank = 2, nrun = 1)


b <- mutate_basis_matrix(fit)




rows <- b %>%
  group_by(contact_type) %>%
  summarise(thunder_bins = list(which(
    (.$feature_score > mean(.$feature_score) + 3*sd(.$feature_score)) |
      (.$std_dev > mean(.$std_dev) + 3*sd(.$std_dev))
  ))) %>%
  tidyr::unnest(thunder_bins) %>%
  ungroup() %>%
  arrange(thunder_bins) %>%
  pull(thunder_bins)

head(mix[rows,])

