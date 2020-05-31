#================================================================
# Evalset methods: show & summarize
#================================================================

#' @name show
#' @title
#' @aliases show,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @export
setGeneric("show",
           valueClass = "",
           function(x){
             standardGeneric("show")
           })
setMethod("show",
          "evalset",
          function(x) {
            cat("\n<Positive set>\n\n")
            show(x@pos_set)
            cat("\n<Negative set>\n\n")
            show(x@neg_set)
            cat("\n<Predicted set>\n\n")
            show(x@pred_set)
            cat("<List of transcription factors>\n")
            cat("First 10 entries of ", length(x@tfs), "\n")
            print(x@tfs[1:10])
          }
)

#' @name summarize
#' @title Generate a table of stats for a given evalset object.
#' @description Generate a table of stats for a given evalset object: sensitivity, specificity, etc.
#' @author Claire Rioualen
#' @param evalset An `evalset` object.
#' @return A dataframe.
#' @export
setGeneric("summarize",
           valueClass = "data.frame",
           function(x){
             standardGeneric("summarize")
           })
setMethod(
  "summarize",
  signature(x = "evalset"),
  function(x) {
    sensitivity <- sensitivity(evalset)
    specificity <- specificity(evalset)
    precision <- precision(evalset)

    data.frame(sensitivity, specificity, precision, stringsAsFactors = F)
  }
)

#================================================================
# Evalset methods: accessors
#================================================================

#' #' @name get_tfs
#' #' @title
#' #' @aliases get_tf,set-method
#' #' @param x An `evalset` object
#' #' @docType methods
#' #' @rdname set-methods
#' #' @return A character vector
#' #' @export
#' setGeneric("get_tfs",
#'            valueClass = "character",
#'            function(x){
#'              standardGeneric("get_tfs")
#'            })
#' setMethod(
#'   "get_tfs",
#'   signature(x = "evalset"),
#'   function(x) {
#'     x@tfs
#'   }
#' )
#'
#' #' @name get_tfs_n
#' #' @aliases get_tf,set-method
#' #' @param x An `evalset` object
#' #' @docType methods
#' #' @rdname set-methods
#' #' @return An integer
#' #' @export
#' setGeneric("get_tfs_n",
#'            valueClass = "integer",
#'            function(x){
#'              standardGeneric("get_tfs_n")
#'            })
#' setMethod(
#'   "get_tfs_n",
#'   signature(x = "evalset"),
#'   function(x) {
#'     length(x@tfs)
#'   }
#' )

#' @name get_sets
#' @aliases get_sets,set-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname set-methods
#' @return A list of `set` objets.
#' @export
setGeneric("get_sets",
           valueClass = "character",
           function(x){
             standardGeneric("get_sets")
           })
setMethod(
  "get_sets",
  signature(x = "evalset"),
  function(x) {
    list(Positive_set=x@pos_set, Negative_set=x@neg_set, Predicted_set=x@pred_set)
  }
)

## should be one accessor per set get_neg_set, get_pos_set, ¡ect

#================================================================
# Evalset methods: base statistics
#================================================================

#' @name true_pos
#' @title
#' @aliases true_pos,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An integer
#' @export
setGeneric("true_pos",
           valueClass = "integer",
           function(x){
             standardGeneric("true_pos")
           })
setMethod(
  "true_pos",
  signature(x = "evalset"),
  function(x) {
    get_ris_n(intersect_by_ris(x@pos_set, x@pred_set))
  }
)

#' @name false_pos
#' @title
#' @aliases false_pos,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An integer
#' @export
setGeneric("false_pos",
           valueClass = "integer",
           function(x){
             standardGeneric("false_pos")
           })
setMethod(
  "false_pos",
  signature(x = "evalset"),
  function(x) {
    get_ris_n(intersect(x@neg_set, x@pred_set))
  }
)

#' @name true_neg
#' @title
#' @aliases true_neg,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An integer
#' @export
setGeneric("true_neg",
           valueClass = "integer",
           function(x){
             standardGeneric("true_neg")
           })
setMethod(
  "true_neg",
  signature(x = "evalset"),
  function(x) {
    get_ris_n(x@neg_set) - get_ris_n(intersect(x@neg_set, x@pred_set))
  }
)

#' @name false_neg
#' @title
#' @aliases false_neg,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An integer
#' @export
setGeneric("false_neg",
           valueClass = "integer",
           function(x){
             standardGeneric("false_neg")
           })
setMethod(
  "false_neg",
  signature(x = "evalset"),
  function(x) {
    get_ris_n(x@pos_set) - get_ris_n(intersect(x@pos_set, x@pred_set))
  }
)

#' @name pred_pos
#' @title
#' @aliases pred_pos,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An integer
#' @export
setGeneric("pred_pos",
           valueClass = "integer",
           function(x){
             standardGeneric("pred_pos")
           })
setMethod(
  "pred_pos",
  signature(x = "evalset"),
  function(x) {
    get_ris_n(intersect(x@pos_set, x@pred_set)) + get_ris_n(intersect(x@neg_set, x@pred_set))
  }
)

#' @name pred_neg
#' @title
#' @aliases pred_neg,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An integer
#' @export
setGeneric("pred_neg",
           valueClass = "integer",
           function(x){
             standardGeneric("pred_neg")
           })
setMethod(
  "pred_neg",
  signature(x = "evalset"),
  function(x) {
    get_ris_n(x@pos_set) + get_ris_n(x@neg_set) - ( get_ris_n(intersect(x@pos_set, x@pred_set)) + get_ris_n(intersect(x@neg_set, x@pred_set)) )
  }
)

#' @name actual_pos
#' @title
#' @aliases actual_pos,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An integer
#' @export
setGeneric("actual_pos",
           valueClass = "integer",
           function(x){
             standardGeneric("actual_pos")
           })
setMethod(
  "actual_pos",
  signature(x = "evalset"),
  function(x) {
    get_ris_n(x@pos_set)
  }
)

#' @name actual_neg
#' @title
#' @aliases actual_neg,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An integer
#' @export
setGeneric("actual_neg",
           valueClass = "integer",
           function(x){
             standardGeneric("actual_neg")
           })
setMethod(
  "actual_neg",
  signature(x = "evalset"),
  function(x) {
    get_ris_n(x@neg_set)
  }
)

#' @name total_pop
#' @title
#' @aliases total_pop,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An integer
#' @export
setGeneric("total_pop",
           valueClass = "integer",
           function(x){
             standardGeneric("total_pop")
           })
setMethod(
  "total_pop",
  signature(x = "evalset"),
  function(x) {
    get_ris_n(x@pos_set) + get_ris_n(x@neg_set)
  }
)

#' @name sensitivity
#' @title
#' @aliases sensitivity,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An numeric
#' @export
setGeneric("sensitivity",
           valueClass = "numeric",
           function(x){
             standardGeneric("sensitivity")
           })
setMethod(
  "sensitivity",
  signature(x = "evalset"),
  function(x) {
    true_pos(x) / actual_pos(x)
  }
)

#' @name specificity
#' @title
#' @aliases specificity,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An numeric
#' @export
setGeneric("specificity",
           valueClass = "numeric",
           function(x){
             standardGeneric("specificity")
           })
setMethod(
  "specificity",
  signature(x = "evalset"),
  function(x) {
    true_neg(x) / actual_neg(x)
  }
)

#' @name precision
#' @title
#' @aliases precision,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An numeric
#' @export
setGeneric("precision",
           valueClass = "numeric",
           function(x){
             standardGeneric("precision")
           })
setMethod(
  "precision",
  signature(x = "evalset"),
  function(x) {
    true_pos(x) / pred_pos(x)
  }
)

#' @name fdr
#' @title
#' @aliases fdr,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An numeric
#' @export
setGeneric("fdr",
           valueClass = "numeric",
           function(x){
             standardGeneric("fdr")
           })
setMethod(
  "fdr",
  signature(x = "evalset"),
  function(x) {
    false_pos(x) / pred_pos(x)
  }
)

#================================================================
# Evalset methods: additional relevant numbers
#================================================================

#' @name outer_set
#' @title
#' @aliases outer_set,evalset-method
#' @description Get the set of RIs in the prediction set that are not in either control set,
#' and thus not taken into account for classic statistics computation
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return A `set` object
#' @export
setGeneric("outer_set",
           valueClass = "set",
           function(x){
             standardGeneric("outer_set")
           })
setMethod(
  "outer_set",
  signature(x = "evalset"),
  function(x) {
    outer_set <- NetworkEval::setdiff(NetworkEval::setdiff(x@pred_set, x@pos_set), x@neg_set)
  }
)

#' @name get_universe
#' @title Get universe of regulatory interactions
#' @description Get set of all possible RIs given the evalset list of TFs and all genes in RegulonDB (!= genes in ensembl)
#' @author Claire Rioualen
#' @param evalset An `evalset` object.
#' @return universe A `set` object of all combinations.
#' @export
#' @import dplyr
setGeneric("get_universe",
           valueClass = "set",
           function(x){
             standardGeneric("get_universe")
           })
setMethod(
  "get_universe",
  signature(x = "evalset"),
  function(x) {
    tfs <- get_tfs(x)
    genes <- get_regulondb_genes()

    all_combinations <- expand.grid(tfs, genes, stringsAsFactors = F)
    colnames(all_combinations) <- c("tf_symbol", "gene_symbol")
    all_combinations <- all_combinations %>% dplyr::distinct() ## should be filtered upstream

    universe <- set(all_combinations, tfs)

    universe
  }
)

#================================================================
# Evalset methods: advanced statistics
#================================================================

#' @name generate_confusion_matrix
#' @title Generate a confusion matrix for a given evalset object.
#' @description Generate a confusion matrix for a given evalset object, composed of a `pset` to be evaluated and a positive and a negative
#' @author Claire Rioualen
#' @param evalset An `evalset` object.
#' @return A dataframe.
#' @export
setGeneric("generate_confusion_matrix",
           function(x){
             standardGeneric("generate_confusion_matrix")
           }
)
setMethod("generate_confusion_matrix",
  signature(x = "evalset"),
  function(x) {
    # validObject(evalset)
    control_positive <- c(true_pos(evalset), false_neg(evalset), actual_pos(evalset))
    control_negative <- c(false_pos(evalset), true_neg(evalset), actual_neg(evalset))
    total_population <- c(pred_pos(evalset), pred_neg(evalset), total_pop(evalset))
    df <- data.frame(control_positive, control_negative, total_population, stringsAsFactors = F)
    row.names(df) <- c("predicted_positive", "predicted_negative", "total_control")
    df
  }
)

#' @name generate_roc_curve
#' @title Generate a ROC curve for a given evalset object.
#' @description Generate a ROC curve for a given evalset object..
#' @author Claire Rioualen
#' @param evalset An evalset object.
#' @return A plot.
#' @export
generate_roc_curve <- function(evalset) {
  ## TODO
}


#' @name generate_pr_curve
#' @title Generate a precision-recall curve for a given evalset object.
#' @description Generate a precision-recall curve for a given evalset object.
#' @author Claire Rioualen
#' @param evalset An evalset object.
#' @return A plot.
#' @export
generate_pr_curve <- function(evalset) {
  ## TODO
}

#' @name generate_venn_diagram
#' @title Generate a Venn diagram using the eulerr package.
#' @description Generate a Venn diagram given a list of Set objects.
#' @author Claire Rioualen
#' @param evalset A list of Set objects.
#' @param style An option in c(1, 2, 3). Default to 1 (classic venn plot)
#' @param universe A logical indicating wether to plot universe of interactions. Defaults to FALSE.
#' @return A plot.
#' @export
#' @import eulerr
generate_venn_diagram <- function(evalset, style=1, universe=FALSE) {

  data <- list()
  for (n in names(get_sets(evalset))) {
    data[[n]] <- paste0(evalset[[n]]@ris$tf_symbol, "_", evalset[[n]]@ris$gene_symbol)
  }
  if (universe==TRUE){
    universe <- get_universe(evalset)
    data[["Universe"]] <- paste0(universe@ris$tf_symbol, "_", universe@ris$gene_symbol)
  }

  if (style == 1) {
    graph <- venn(data)
  } else if (style == 2) {
    graph <- euler(data, shape = "circle")
  }  else if (style == 3) {
    graph <- euler(data, shape = "ellipse")
  }
  plot(graph, quantities = TRUE)
}

#================================================================
# Evalset methods: generate report
#================================================================

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

