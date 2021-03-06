% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_set-methods.R
\name{subset_by_tfs}
\alias{subset_by_tfs}
\title{Get a subset of interactions from a Set object given a list of TFs.}
\usage{
subset_by_tfs(set, tfs)
}
\arguments{
\item{set}{A set object.}

\item{tfs}{A character vector of TF names.}
}
\value{
A Set object.
}
\description{
Given a list of transcription factors, extract the interactions of those TFs in a given Set.
}
\author{
Claire Rioualen
}
