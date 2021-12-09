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
