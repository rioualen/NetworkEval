#================================================================
# Set methods: show & summarize
#================================================================

#' @title The show methods for set objects
#' @name show
#' @aliases show,set-method
#' @param x A `set` object
#' @docType methods
#' @rdname set-methods
#' @export
setGeneric("show",
           function(object){
             standardGeneric("show")
           }
)
setMethod("show",
          "set",
          function(object) {
            cat("<List of interactions>\n")
            cat("First 10 entries of ", nrow(object@ris), "\n")
            print(object@ris[1:10,])
            cat("<List of transcription factors>\n")
            cat("First 10 entries of ", length(object@tfs), "\n")
            print(object@tfs[1:10])
          }
)

#' @title The show methods for cset objects
#' @name show
#' @aliases show,set-method
#' @param x A `cset` object
#' @docType methods
#' @rdname set-methods
#' @export
setGeneric("show",
           function(object){
             standardGeneric("show")
           }
)
setMethod("show",
          "cset",
          function(object) {
            cat("<List of interactions>\n")
            cat("First 10 entries of ", nrow(object@ris), "\n")
            print(object@ris[1:10,])
            cat("<List of transcription factors>\n")
            cat("First 10 entries of ", length(object@tfs), "\n")
            print(object@tfs[1:10])
            cat("<Type of set>\n")
            cat(object@type, "\n")
            cat("<ID of set>\n")
            cat(object@id, "\n")
          }
)

#' @title The show methods for pset objects
#' @name show
#' @aliases show,set-method
#' @param x A `pset` object
#' @docType methods
#' @rdname set-methods
#' @export
setGeneric("show",
           function(object){
             standardGeneric("show")
           }
)
setMethod("show",
          "pset",
          function(object) {
            cat("<List of interactions>\n")
            cat("First 10 entries of ", nrow(object@ris), "\n")
            print(object@ris[1:10,])
            cat("<List of transcription factors>\n")
            cat("First 10 entries of ", length(object@tfs), "\n")
            print(object@tfs[1:10])
            cat("<List of scores>\n")
            cat("First 10 entries of ", nrow(object@scores), "\n")
            print(object@scores[1:10,])
          }
)

#' @title Generate a dataframe of sets properties.
#' @description Generate a dataframe of sets properties.
#' @author Claire Rioualen
#' @param set A Set object.
#' @return A dataframe.
#' @export
setGeneric("summarize",
           valueClass = "data.frame",
           function(x){
             standardGeneric("summarize")
           })
setMethod(
  "summarize",
  signature(x = "set"),
  function(x) {
    data.frame(RI_number = get_ris_n(x), TF_number = get_tfs_n(x))
  })

#================================================================
# Set methods: accessors
#================================================================

#' @title Get list of RIS from a set object.
#' @name get_ris
#' @aliases get_ris,set-methods
#' @param x A `set` object
#' @docType methods
#' @rdname set-methods
#' @return A df
#' @export
setGeneric("get_ris",
           valueClass = "data.frame",
           function(x){
             standardGeneric("get_ris")
           })
setMethod(
  "get_ris",
  signature(x = "set"),
  function(x) {
    x@ris
  }
)

#' @title Get numbers of RIs from a set object.
#' @name get_ris_n
#' @aliases get_ris_n,set-methods
#' @param x A `set` object
#' @docType methods
#' @rdname set-methods
#' @return An integer
#' @export
setGeneric("get_ris_n",
           valueClass = "integer",
           function(x){
             standardGeneric("get_ris_n")
           })
setMethod(
  "get_ris_n",
  signature(x = "set"),
  function(x) {
    nrow(x@ris)
  }
)

#' @title Get list of TFs from a set object.
#' @name get_tfs
#' @aliases get_tfs,set-methods
#' @param x A `set` object
#' @docType methods
#' @rdname set-methods
#' @return A character vector
#' @export
setGeneric("get_tfs",
           valueClass = "character",
           function(x){
             standardGeneric("get_tfs")
           })
setMethod(
  "get_tfs",
  signature(x = "set"),
  function(x) {
    x@tfs
  }
)

#' @title Get number TFs from a set object.
#' @name get_tfs_n
#' @aliases get_tfs_n,set-methods
#' @param x A `set` object
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
  signature(x = "set"),
  function(x) {
    length(x@tfs)
  }
)


#================================================================
# Set methods: set operations
#================================================================

#' @title Intersect 2 sets by RIs.
#' @name intersect_by_ris
#' @aliases intersect_by_ris,set-method
#' @param x A `set` object
#' @param y A `set` object
#' @docType methods
#' @rdname set-methods
#' @return A [set][NetworkEval::set] object.
#' @import dplyr
#' @export
setGeneric("intersect_by_ris",
           valueClass = "integer",
           function(x, y){
             standardGeneric("intersect_by_ris")
           })
setMethod(
  "intersect_by_ris",
  signature(x = "set", y = "set"),
  function(x, y) {
    stopifnot(identical(get_tfs(x), get_tfs(y)))

    ris <- dplyr::intersect(x@ris, y@ris)
    tfs <- x@tfs
    set(ris, tfs)
  }
)

#' @title Union 2 sets by RIs
#' @name union_by_ris
#' @aliases union_by_ris,set-method
#' @param x A `set` object
#' @param y A `set` object
#' @docType methods
#' @rdname set-methods
#' @return A [set][NetworkEval::set] object.
#' @import dplyr
#' @export
setGeneric("union_by_ris",
           valueClass = "integer",
           function(x, y){
             standardGeneric("union_by_ris")
           })
setMethod(
  "union_by_ris",
  signature(x = "set", y = "set"),
  function(x, y) {
    stopifnot(identical(get_tfs(x), get_tfs(y)))

    ris <- dplyr::union(x@ris, y@ris)
    tfs <- x@tfs
    set <- set(ris, tfs)
    set
  }
)

#' @title Setdiff between 2 sets by RIs
#' @name setdiff_by_ris
#' @aliases setdiff_by_ris,set-method
#' @param x A `set` object
#' @param y A `set` object
#' @docType methods
#' @rdname set-methods
#' @return A [set][NetworkEval::set] object.
#' @import dplyr
#' @export
setGeneric("setdiff_by_ris",
           valueClass = "integer",
           function(x, y){
             standardGeneric("setdiff_by_ris")
           }
)
setMethod(
  "setdiff_by_ris",
  signature(x = "set", y = "set"),
  function(x, y) {
    stopifnot(identical(get_tfs(x), get_tfs(y)))

    ris <- dplyr::setdiff(x@ris, y@ris)
    tfs <- x@tfs
    set <- set(ris, tfs)
    set
  }
)

#' @title Get a subset of interactions from a Set object given a list of TFs.
#' @description Given a list of transcription factors, extract the interactions of those TFs in a given Set.
#' @author Claire Rioualen
#' @param set A set object.
#' @param tfs A character vector of TF names.
#' @return A Set object.
#' @export
setGeneric("subset_by_tfs",
           valueClass = "set",
           function(set, tfs){
             standardGeneric("subset_by_tfs")
           }
)
setMethod(
  "subset_by_tfs",
  signature(set = "set", tfs = "character"),
  function(set, tfs) {
    ris <- set@ris %>% filter(tf_symbol %in% tfs)
    tfs <- intersect(set@tfs, tfs)

    set(ris, tfs)         ## Maybe issue a warning if one or several TFs are absent from the set to be subsetted
  }
)