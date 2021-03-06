% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02_evalset-methods.R
\name{output_files}
\alias{output_files}
\title{Create a directory with tables and figures.}
\usage{
output_files(x)
}
\arguments{
\item{x}{An `evalset` object}
}
\description{
Create a directory with tables and figures.
}
\details{
#' @name write_report
#' @title Generate a report of the analyses.
#' @description Generate a report summarizing all the statistics en graphes generated for one or several evaluations.
#' @author Claire Rioualen
#' @param eval_list An list of Evaluation objects.
#' @return A plot.
#' @import rmarkdown
#' @export
setGeneric("write_report",
           function(x){
             standardGeneric("write_report")
           })
setMethod(
  "write_report",
  signature(x = "evalset"),
  function(x) {
    # validObject(evalset)
    rmarkdown::render("Report.Rmd", params = list(
      evalset = x
      ))
  }
)
}
\author{
Claire Rioualen
}
