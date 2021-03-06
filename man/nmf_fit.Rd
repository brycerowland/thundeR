% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nmf_fit.R
\name{nmf_fit}
\alias{nmf_fit}
\title{NMF fit for mixture matrix of Hi-C data}
\usage{
nmf_fit(mixture, n_cell_types = 2, itter = 200)
}
\arguments{
\item{mixture}{Input mixture matrix for deconvolution where rows are Hi-C features and columns are samples.}

\item{n_cell_types}{Integer. The number of columns in the basis matrix of the deconvolution. Corresponds to the number of cell types in bulk Hi-C mixture.}

\item{itter}{Integer. Number of itterations for NMF algorithm. Default is 200.}
}
\description{
NMF fit for mixture matrix of Hi-C data
}
