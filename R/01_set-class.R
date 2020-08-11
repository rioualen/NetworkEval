#================================================================
# The Set class
#================================================================
#' @title The Set class
#' @slot ris A 2-column character dataframe
#' @slot tfs A character vector of TF names
#' @import dplyr
#' @export
setClass(
  "set",
  slots = list(
    ris = "data.frame",
    tfs = "character"
  )
)
#' @title Constructor function of a set object.
#' @description Constructor function of a set object.
#' @param ris A 2-column character dataframe of TF-gene interactions.
#' @param tfs A character vector of TF names.
#' @return A `set` object.
#' @export
set <-  function(ris, tfs) {
  new("set", ris = ris, tfs = tfs)
}
#' #' @title The Set class
#' #' @description Creates a `set` object that represents a set of TF-gene interactions.
#' #' It is composed of 2 slots: a 2-column character dataframe named `ris` (regulatory interactions) and
#' #' a character vector named `tfs` (transcription factors).
#' #'
#' #' @slot ris A 2-column character dataframe of TF-gene interactions with colnames = c("tf.symbol", "gene.symbol")
#' #' @slot tfs A character vector of TF names
#' #' @import dplyr
#' #' @export
#'
#' setClass(
#'   "set",
#'   slots = list(
#'     ris = "data.frame",
#'     tfs = "character"
#'   )
#' )
#'
#' # setValidity("set", function(object) {
#' #   ## TODO
#' #   ## tfs have to be ordered and unique
#' # })
#'
#' #' @title Constructor function of a set object.
#' #' @description Constructor function of a set object.
#' #' @param ris A 2-column character dataframe of TF-gene interactions.
#' #' @param tfs A character vector of TF names.
#' #' @return A `set` object.
#' #' @export
#'
#' set <-  function(ris, tfs) {
#'   stopifnot(is.data.frame(ris))
#'   stopifnot(is.character(tfs))
#'   ris %>% dplyr::distinct() ## maybe issue a warning in case tfs and or ris are not unique?
#'   tfs <- unique(sort(tfs))
#'
#'   new("set", ris = ris, tfs = tfs)
#' }

#================================================================
# The CSet class
#================================================================

#' @title The CSet class
#' @description Creates a `cset` object that represents a control set of TF-gene interactions. It inherits from the `set` class.
#' It is composed of 4 slots: a 2-column character dataframe named `ris` (regulatory interactions) and
#' a character vector named `tfs` (transcription factors) inherited from the Set class,
#' a character attribute `type` and a character attribute `id`.
#' @slot ris A 2-column character dataframe of TF-gene interactions with colnames = c("tf.symbol", "gene.symbol")
#' @slot tfs A character vector of TF names
#' @slot type A character in c("positive", "negative")
#' @slot id A character name
#' @export

setClass(
  "cset",
  contains="set",
  slots = list(
    ris = "data.frame",
    tfs = "character",
    type = "character",
    id = "character"
  )
)

# setValidity(
#   ## todo
# )

#' @title Constructor function of a cset object.
#' @description Constructor function of a set object.
#' @param set A `set` object
#' @param type A character in c("positive", "negative")
#' @param id A character name
#' @return A `cset` object.
#' @export

cset <-  function(set, type, id) {
  # stopifnot(is.data.frame(ris))
  stopifnot(is.character(type))
  stopifnot(is.character(id))
  stopifnot(type %in% c("positive", "negative"))

  new("cset", ris = set@ris, tfs = set@tfs, type = type, id = id)

}

#================================================================
# The PSet class
#================================================================

#' @title The PSet class
#' @description Creates a `pset` object that represents a prediccted set of TF-gene interactions. It inherits from the `set` class.
#' It is composed of 3 slots: a 2-column character dataframe named `ris` (regulatory interactions) and
#' a character vector named `tfs` (transcription factors) inherited from the Set class,
#' and a 1-column dataframe containing scores associated with each regulatory interaction.
#' @slot ris A 2-column character dataframe of TF-gene interactions with colnames = c("tf.symbol", "gene.symbol")
#' @slot tfs A character vector of TF names
#' @slot scores A dataframe of 1 column with a number of rows equal to that of set@ris.
#' @export
setClass(
  "pset",
  contains = "set",
  slots = list(
    ris = "data.frame",
    tfs = "character",
    scores = "character",
    id = "character"
  )
)

# setValidity(
#   ## todo
# )

#' @title Constructor function of a pset object.
#' @description Constructor function of a set object.
#' @param set A `set` object.
#' @param scores A dataframe of 1 column with a number of rows equal to that of set@ris.
#' @return A `pset` object.
#' @export

pset <-  function(set, scores, id) {
  # stopifnot(is.numeric(scores))

  new("pset", ris = set@ris, tfs = set@tfs, scores = scores, id = id)

}

