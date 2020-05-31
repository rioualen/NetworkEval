#================================================================
# The EvalSet class
#================================================================

#' @title The EvalSet class
#' @description Performs the evaluation of a given input set against selected controls sets (1 positive and 1 negative).
#' It is thus composed of 3 input `set` objects.
#'
#' @slot pos_set A cset object with type = "positive".
#' @slot neg_set A cset object with type = "negative".
#' @slot pred_set A pset object containing predictions to be evaluated against controls.
#' @export

setClass(
  "evalset",
  contains=c("set", "cset", "pset"),
  slots = list(
    pos_set = "cset",
    neg_set = "cset",
    pred_set = "pset",
    tfs = "character"
  )
)

# setValidity(
#   # TODO
#   # all TFs must be equal between the 3 sets
#   # all 3 sets have to be valid sets
#   # ...
# )

#' @title Constructor function of the EvalSet object
#' @description Constructor function of an EvalSet object in order to perform the evaluation of a predicted set against control sets.
#' @param pos_set A set object to be used a positive control.
#' @param neg_set A set object to be used a negative control.
#' @param pred_set A set object containing predictions to be evaluated against controls.
#' @return An `evalset` object.
#' @export

evalset <-  function(pos_set, neg_set, pred_set) {
  stopifnot(is.data.frame(ris))
  stopifnot(identical(get_tfs(pos_set), get_tfs(neg_set)))
  stopifnot(identical(get_tfs(pos_set), get_tfs(pred_set)))

  tfs <- get_tfs(pos_set)
  new("evalset", pos_set = pos_set, neg_set = neg_set, pred_set = pred_set, tfs = tfs)
}





