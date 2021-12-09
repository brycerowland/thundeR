# thundeR

THUNDER is a deconvolution method specifically tailored to address unique challenges of estimating cell type proportions in Hi-C data. Our algorithm is a two-step deconvolution approach based off of NMF, which first performs feature selection specific to the type of Hi-C readout, then provides estimated cell type proportions using only informative bin-pairs. 


## Installing and Running THUNDER
THUNDER can be installed using the `devtools` R package with the following code. 

```
if(!require("devtools")){install.packages("devtools")}
if(!require("NMF")){install.packages("NMF")}
devtools::install_github("https://github.com/brycerowland/thundeR.git")
library(thundeR)
```

Estimating cell type proportions from bulk Hi-C data can be performed using the `run_thunder` function as follows:

```
run_thunder(path_to_mixture = "tests/testthat/new_cols_test_data2.tsv", 
            n_cell_types = 2, itter = 1,
            subset_mix_out_path = "/path/to/out_file/outfile.txt.gz", 
            out_init_nmf = "/path/to/out_init_nmf_fit/init_nmf_fit.RDS")
```

where `path_to_mixture` is the file path to the input data (see below for formatting requirements), `n_cell_types` is the number of cell types assumed in the Hi-C mixtures, `itter` is the total number of itterations to run the NMF algorithm (default is 200), `subset_mix_out_path` is a required argument to output the subset mixture matrix after an initial feature selection step, and `out_init_nmf` is an optional argument that is a file path to output the initial NMF object estimated from step 1 of THUNDER. 

## Mixture data format
THUNDER assumes that mixtures of Hi-C data are stored in long data format where the first row of the file are column names with the following specifications. 
 + The first column of the file is `feature_name` and values in this column describe Hi-C data features.
 + The second column is `contact_type` which describes whether the feature is derived from `intra`-chromosomal contacts or `inter`-chromosomal contacts. THUNDER performs feature selection seperately for `intra` and `inter` features. 
 + The remaining columns are assumed to be data from a Hi-C experiment stored in long format, where each column is a seperate sample. 
