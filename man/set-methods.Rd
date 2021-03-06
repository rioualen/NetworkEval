% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01_set-methods.R, R/02_evalset-methods.R
\docType{methods}
\name{show}
\alias{show}
\alias{show,set-methods}
\alias{show,set-method}
\alias{get_ris}
\alias{get_ris,set-methods}
\alias{get_ris_n}
\alias{get_ris_n,set-methods}
\alias{get_tfs_set}
\alias{get_tfs_set,set-methods}
\alias{get_tfs_n}
\alias{get_tfs_n,set-methods}
\alias{intersect_by_ris}
\alias{intersect_by_ris,set-method}
\alias{union_by_ris}
\alias{union_by_ris,set-method}
\alias{setdiff_by_ris}
\alias{setdiff_by_ris,set-method}
\alias{get_tfs_eval}
\alias{get_tf,set-method}
\alias{get_sets}
\alias{get_sets,set-methods}
\title{The show methods for set objects}
\usage{
show(x)

show(x)

show(x)

get_ris(x)

get_ris_n(x)

get_tfs_set(x)

get_tfs_n(x)

intersect_by_ris(x, y)

union_by_ris(x, y)

setdiff_by_ris(x, y)

get_tfs_eval(x)

get_sets(x)
}
\arguments{
\item{x}{An `evalset` object}

\item{y}{A `set` object}

\item{cset}{A `cset` object}

\item{pset}{A `pset` object}
}
\value{
A df

An integer

A character vector

An integer

A [set][NetworkEval::set] object.

A [set][NetworkEval::set] object.

A [set][NetworkEval::set] object.

A character vector

A list of `set` objets.
}
\description{
The show methods for set objects

The show methods for cset objects

The show methods for pset objects

Get list of RIS from a set object.

Get numbers of RIs from a set object.

Get list of TFs from a set object.

Get number TFs from a set object.

Intersect 2 sets by RIs.

Union 2 sets by RIs

Setdiff between 2 sets by RIs

Accessor function to get list of TFs fron evalset objet.

#' @name get_tfs_n
#' @aliases get_tf,set-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname set-methods
#' @return An integer
#' @export
setGeneric("get_tfs_n",
           valueClass = "integer",
           function(x){
             standardGeneric("get_tfs_n")
           })
setMethod(
  "get_tfs_n",
  signature(x = "evalset"),
  function(x) {
    length(x@tfs)
  }
)
}
