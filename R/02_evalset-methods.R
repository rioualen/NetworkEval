#================================================================
# Evalset methods: show & summarize
#================================================================

#' @name summarize_stats
#' @title Generate a table of stats for a given evalset object.
#' @description Generate a table of stats for a given evalset object: sensitivity, specificity, etc.
#' @author Claire Rioualen
#' @param evalset An `evalset` object.
#' @return A dataframe.
#' @export
setGeneric("summarize_stats",
           valueClass = "data.frame",
           function(x){
             standardGeneric("summarize_stats")
           })
setMethod(
  "summarize_stats",
  signature(x = "evalset"),
  function(x) {
    sensitivity <- sensitivity(x)
    specificity <- specificity(x)
    precision <- precision(x)

    data.frame(sensitivity, specificity, precision, stringsAsFactors = F)
  }
)

#================================================================
# Evalset methods: accessors
#================================================================

#' @name get_tfs_eval
#' @title Accessor function to get list of TFs fron evalset objet.
#' @aliases get_tf,set-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname set-methods
#' @return A character vector
#' @export
setGeneric("get_tfs_eval",
           valueClass = "character",
           function(x){
             standardGeneric("get_tfs_eval")
           })
setMethod(
  "get_tfs_eval",
  signature(x = "evalset"),
  function(x) {
    x@tfs
  }
)
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
#' @aliases get_sets,set-methods
#' @param x An `evalset` object
#' @docType methods
#' @rdname set-methods
#' @return A list of `set` objets.
#' @export
setGeneric("get_sets",
           valueClass = "list",
           function(x){
             standardGeneric("get_sets")
           }
)
setMethod(
  "get_sets",
  signature(x = "evalset"),
  function(x) {
    list(pos_set=x@pos_set, neg_set=x@neg_set, pred_set=x@pred_set)
  }
)

#================================================================
# Evalset methods: base statistics
#================================================================

#' @name true_pos
#' @title Get TP number
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
#' @title Get false positive number
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
    get_ris_n(intersect_by_ris(x@neg_set, x@pred_set))
  }
)

#' @name true_neg
#' @title Get TN number
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
    get_ris_n(x@neg_set) - get_ris_n(intersect_by_ris(x@neg_set, x@pred_set))
  }
)

#' @name false_neg
#' @title Get FN number
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
    get_ris_n(x@pos_set) - get_ris_n(intersect_by_ris(x@pos_set, x@pred_set))
  }
)

#' @name pred_pos
#' @title Get PP number
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
    get_ris_n(intersect_by_ris(x@pos_set, x@pred_set)) + get_ris_n(intersect_by_ris(x@neg_set, x@pred_set))
  }
)

#' @name pred_neg
#' @title Get negative prediction number
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
    get_ris_n(x@pos_set) + get_ris_n(x@neg_set) - ( get_ris_n(intersect_by_ris(x@pos_set, x@pred_set)) + get_ris_n(intersect_by_ris(x@neg_set, x@pred_set)) )
  }
)

#' @name actual_pos
#' @title Get actual positives number
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
#' @title Get actual negative number
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
#' @title Get total population number
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
#' @title Get sensitivity value
#' @aliases sensitivity,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return A numeric
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
#' @title Get specificity
#' @aliases specificity,evalset-method
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return A numeric
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
#' @title Get precision value
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
#' @title Get FDR value
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
#' @title Get outer set size
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
    outer_set <- setdiff_by_ris(setdiff_by_ris(x@pred_set, x@pos_set), x@neg_set)
  }
)

#' @name inner_set
#' @title Get nner set size
#' @aliases outer_set,evalset-method
#' @description Get the set of RIs in the prediction set that are not in either control set,
#' and thus not taken into account for classic statistics computation
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return A `set` object
#' @export
setGeneric("inner_set",
           valueClass = "set",
           function(x){
             standardGeneric("inner_set")
           })
setMethod(
  "inner_set",
  signature(x = "evalset"),
  function(x) {
    inner_set <- union_by_ris(intersect_by_ris(x@pred_set, x@pos_set), intersect_by_ris(x@pred_set, x@neg_set))
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
#' @import EcoliGenes
setGeneric("get_universe",
           valueClass = "set",
           function(x){
             standardGeneric("get_universe")
           })
setMethod(
  "get_universe",
  signature(x = "evalset"),
  function(x) {
    tfs <- get_tfs_eval(x)
    genes <- EcoliGenes::get_target_genes()

    all_combinations <- expand.grid(tfs, genes, stringsAsFactors = F)
    colnames(all_combinations) <- c("tf_bnum", "gene_bnum")
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
#' @param x An `evalset` object.
#' @return A dataframe.
#' @export
setGeneric("generate_confusion_matrix",
           valueClass = "data.frame",
           function(x){
             standardGeneric("generate_confusion_matrix")
           }
)
setMethod("generate_confusion_matrix",
  signature(x = "evalset"),
  function(x) {
    # validObject(evalset)
    control_positive <- c(true_pos(x), false_neg(x), actual_pos(x))
    control_negative <- c(false_pos(x), true_neg(x), actual_neg(x))
    total_population <- c(pred_pos(x), pred_neg(x), total_pop(x))
    df <- data.frame(control_positive, control_negative, total_population, stringsAsFactors = F)
    row.names(df) <- c("predicted_positive", "predicted_negative", "total_control")
    df
  }
)

#' @name stats_per_tf
#' @title Get TPR per TF
#' @description Get TPR per TF
#' @author Claire Rioualen
#' @param evalset An evalset object.
#' @return A dataframe
#' @import dplyr
#' @export
setGeneric("stats_per_tf",
           valueClass = "data.frame",
           function(x){
             standardGeneric("stats_per_tf")
           }
)
setMethod("stats_per_tf",
          signature(x = "evalset"),
          function(x) {
            tfs <- x@pred_set@tfs

            pos <- x@pos_set@ris
            pred <- x@pred_set@ris

            pred <- pred %>% dplyr::mutate(pair=paste0(tf_bnum, "_", gene_bnum))

            pos <- pos %>% dplyr::mutate(pair=paste0(tf_bnum, "_", gene_bnum)) %>% dplyr::mutate(labels = ifelse(pair %in% pred$pair,1,0)) %>%
              dplyr::select(-pair)


            tab <- pos %>% dplyr::group_by(tf_bnum) %>% dplyr::mutate(ss=mean(labels), tp=sum(labels), ap=n()) %>% dplyr::select(-gene_bnum, -labels) %>% distinct()

            tab
          }
)

#' @name generate_roc_curve
#' @title Generate a ROC curve for a given evalset object.
#' @description Generate a ROC curve for a given evalset object..
#' @author Claire Rioualen
#' @param evalset An evalset object.
#' @param score An character vector indicating which score to process.
#' @return A list containing a plot and a dataframe
#' @import rlist
#' @import reshape2
#' @import ggplot2
#' @import plotROC
#' @import dplyr
#' @export
setGeneric("generate_roc_curve",
           valueClass = "list",
            function(x){
             standardGeneric("generate_roc_curve")
           }
)
setMethod("generate_roc_curve",
          signature(x = "evalset"),
          function(x) {
            pos <- x@pos_set@ris
            neg <- x@neg_set@ris
            pred_data <- x@pred_set@ris

            score_names <- x@pred_set@scores

            scores <- pred_data %>% dplyr::select(all_of(score_names))

            pos <- pos %>% dplyr::mutate(pair=paste0(tf_bnum, "_", gene_bnum))
            neg <- neg %>% dplyr::mutate(pair=paste0(tf_bnum, "_", gene_bnum))

            pred_data <- pred_data %>% dplyr::mutate(pair=paste0(tf_bnum, "_", gene_bnum)) %>% dplyr::mutate(labels = ifelse(pair %in% pos$pair,1,ifelse(pair %in% neg$pair,0,NA))) %>%
              dplyr::select(-pair)

            long_data <- reshape2::melt(pred_data %>% dplyr::select(!!score_names, labels), id="labels")

            ggroc <- ggplot(long_data, aes(m = value, d = labels, color = variable)) +
              geom_roc(labels = F, size=0.5, n.cuts = 0) +
              style_roc(theme = theme_grey)

            AUC <- round(calc_auc(ggroc)$AUC, 3)
            colors <- scales::hue_pal()(13)

            ggroc <- ggroc +
              scale_color_manual(labels = paste0(levels(long_data$variable), " (AUC = ", AUC, ")"), values = colors, name = "Score type") +
              theme(axis.text=element_text(size=14), axis.title=element_text(size=14), legend.text=element_text(size=12))

            auc <- cbind.data.frame(score_names, AUC)
            list(curve=ggroc, auc=auc)
          }
)

#' @name generate_prc_curve
#' @title Generate a precision-recall curve for a given evalset object.
#' @description Generate a precision-recall curve for a given evalset object.
#' @author Claire Rioualen
#' @param evalset An evalset object.
#' @param score An character vector indicating which score to process.
#' @return A list containing a plot and a dataframe
#' @import rlist
#' @import ggplot2
#' @import precrec
#' @import dplyr
#' @export
setGeneric("generate_prc_curve",
           valueClass = "list",
           function(x){
             standardGeneric("generate_prc_curve")
           }
)
setMethod("generate_prc_curve",
          signature(x = "evalset"),
          function(x) {
            pos <- x@pos_set@ris
            neg <- x@neg_set@ris
            pred_data <- x@pred_set@ris

            score_names <- x@pred_set@scores
            scores <- pred_data %>% dplyr::select(all_of(score_names))

            pos <- pos %>% dplyr::mutate(pair=paste0(tf_bnum, "_", gene_bnum))
            neg <- neg %>% dplyr::mutate(pair=paste0(tf_bnum, "_", gene_bnum))

            pred_data <- pred_data %>% dplyr::mutate(pair=paste0(tf_bnum, "_", gene_bnum)) %>% dplyr::mutate(labels = ifelse(pair %in% pos$pair,1,ifelse(pair %in% neg$pair,0,NA))) %>%
              dplyr::select(-pair) %>% dplyr::filter(! is.na(labels))

            colors <- scales::hue_pal()(13)

            labels <- pred_data$labels
            scores <- pred_data %>% select(!!score_names)

            mdat <- mmdata(scores, labels, modnames=score_names)
            eval <- evalmod(mdat)
            aucs <- auc(eval)
            aucs <- subset(aucs, curvetypes == "PRC") %>% select(modnames, aucs) %>% rename(scores = modnames, auc = aucs)
            AUC <- round(aucs$auc, 3)

            plot <- autoplot(eval, "PRC") + scale_color_manual(labels = paste0(score_names, " (AUC = ", AUC, ")"), values = colors, name = "Score type")

            list(curve=plot, auc=aucs)
          }
)

#' @name generate_venn_diagram
#' @title Generate a Venn diagram using the eulerr package.
#' @description Generate a Venn diagram given a list of Set objects.
#' @author Claire Rioualen
#' @param x An evalset object
#' @param style An option in c(1, 2, 3). Default to 1 (classic venn plot)
#' @param universe A logical indicating wether to plot universe of interactions. Defaults to FALSE.
#' @return A plot.
#' @export
#' @import eulerr
setGeneric("generate_venn_diagram",
           valueClass = "eulergram",
           function(x, style, universe){
             standardGeneric("generate_venn_diagram")
           }
)
setMethod("generate_venn_diagram",
          signature(x = "evalset"),
          function(x, style=1, universe=FALSE) { ## default do not work?

            data <- list()
            for (n in names(get_sets(x))) {
              data[[n]] <- paste0(slot(x, n)@ris$tf_bnum, "_", slot(x, n)@ris$gene_bnum)
            }
            if (universe==TRUE){
              universe <- get_universe(x)
              data[["universe"]] <- paste0(universe@ris$tf_bnum, "_", universe@ris$gene_bnum)
            }

            data$pred_set <- unique(data$pred_set) ###### temp hay que ver que pedo con las duplicaciones
            if (style == 1) {
              graph <- venn(data)
            } else if (style == 2) {
              graph <- euler(data, shape = "circle")
            }  else if (style == 3) {
              graph <- euler(data, shape = "ellipse")
            }
            plot(graph, quantities = TRUE)
          }
)

#' @name shuffle_scores
#' @title Shuffle scores
#' @aliases shuffle_scores,evalset-method
#' @description Shuffle scores of predicted set, each score column independently
#' @param x An `evalset` object
#' @docType methods
#' @rdname evalset-methods
#' @return An `evalset` object
#' @export
setGeneric("shuffle_scores",
           valueClass = "evalset",
           function(x){
             standardGeneric("shuffle_scores")
           })
setMethod(
  "shuffle_scores",
  signature(x = "evalset"),
  function(x) {
    ## method 1 - shuffle score columns individually
    for (score in x@pred_set@scores) {
      x@pred_set@ris <- x@pred_set@ris %>% mutate(!!score := sample(get(score)))#  !!as.name(score) = sample(!!as.name(score)))
    }
    ## method 2 - shuffle all score together
    # set.seed(42)
    # score_df <- x@pred_set@ris %>% select(x@pred_set@scores)
    # rows <- sample(nrow(score_df))
    # diamonds <- score_df[rows, ]
    # shuffled_set <- cbind.data.frame(x@pred_set@ris[, c("tf_bnum", "gene_bnum")], diamonds)
    # x@pred_set@ris <- shuffled_set

    evalset(pos_set = x@pos_set, neg_set = x@neg_set, pred_set = x@pred_set, out_tfs = x@out_tfs, out_ris = x@out_ris)
  }
)

#================================================================
# Evalset methods: generate report
#================================================================

#' #' @name write_report
#' #' @title Generate a report of the analyses.
#' #' @description Generate a report summarizing all the statistics en graphes generated for one or several evaluations.
#' #' @author Claire Rioualen
#' #' @param eval_list An list of Evaluation objects.
#' #' @return A plot.
#' #' @import rmarkdown
#' #' @export
#' setGeneric("write_report",
#'            function(x){
#'              standardGeneric("write_report")
#'            })
#' setMethod(
#'   "write_report",
#'   signature(x = "evalset"),
#'   function(x) {
#'     # validObject(evalset)
#'     rmarkdown::render("Report.Rmd", params = list(
#'       evalset = x
#'       ))
#'   }
#' )

#' @name output_files
#' @title Create a directory with tables and figures.
#' @description Create a directory with tables and figures.
#' @author Claire Rioualen
#' @param x An `evalset` object
#'
#' @import gridExtra
#' @import Cairo
#' @export
output_files <- function(x) {

  ## Create results directory if it doesn't exist already
  dir.create(file.path(getwd(), "results"))

  ## Create directory of the same name as the input file
  id_set <- x@pred_set@id
  dir.create(file.path(getwd(), "results", id_set))

  ## Create tables summarizing the number of TFs and RIs from the input file and after TF selection

  in_stats <- data.frame(RIs = get_ris_n(x@pred_set) + nrow(x@out_ris), TFs = get_tfs_n(x@pred_set) + length(x@out_tfs))
  out_stats <- summarize(x@pred_set)

  write.table(in_stats, file=paste0("results/", id_set, "/", id_set, "_in_stats.tsv"), quote = F, col.names = T, row.names = F, sep = "\t")
  write.table(out_stats, file=paste0("results/", id_set, "/", id_set, "_out_stats.tsv"), quote = F, col.names = T, row.names = F, sep = "\t")

  tfs_in <- EcoliGenes::bnumber_to_symbol(get_tfs_eval(x))
  tfs_out <- EcoliGenes::bnumber_to_symbol(x@out_tfs)

  write.table(sort(tfs_in), file=paste0("results/", id_set, "/", id_set, "_tfs_in.tsv"), quote = F, col.names = F, row.names = F, sep = "\t")
  if (length(tfs_out>=1)) { write.table(sort(tfs_out), file=paste0("results/", id_set, "/", id_set, "_tfs_out.tsv"), quote = F, col.names = F, row.names = F, sep = "\t") }

  ## Create tables with stats results

  write.table(summarize_stats(x), file=paste0("results/", id_set, "/", id_set, "_statistics.tsv"), quote = F, col.names = T, row.names = F, sep = "\t")
  write.table(generate_confusion_matrix(x), file=paste0("results/", id_set, "/", id_set, "_confusion_matrix.tsv"), quote = F, col.names = T, row.names = F, sep = "\t")
  write.table(stats_per_tf(x), file=paste0("results/", id_set, "/", id_set, "_stats_per_tf.tsv"), quote = F, col.names = T, row.names = F, sep = "\t")

  ## Save figures

  pdf(paste0("results/", id_set, "/", id_set, "_venn.pdf"))
  gridExtra::grid.arrange(generate_venn_diagram(x, style=1, universe=FALSE))
  dev.off()

  if (length(x@pred_set@scores) >= 1) {
    # pdf(paste0("results/", id_set, "/", id_set, "_roc.pdf"))
    Cairo::Cairo(
      20, #length
      20, #width
      file = paste0("results/", id_set, "/", id_set, "_roc.png"),
      type = "png", #tiff
      bg = "transparent", #white or transparent depending on your requirement
      dpi = 200,
      units = "cm" #you can change to pixels etc
    )
    roc <- generate_roc_curve(x)
    plot(roc$curve)
    dev.off()
  }

  if (length(x@pred_set@scores) >= 1) {
    # pdf(paste0("results/", id_set, "/", id_set, "_prc.pdf"))
    # png(paste0("results/", id_set, "/", id_set, "_prc.png"))

    Cairo::Cairo(
      20, #length
      20, #width
      file = paste0("results/", id_set, "/", id_set, "_prc.png"),
      type = "png", #tiff
      bg = "transparent", #white or transparent depending on your requirement
      dpi = 200,
      units = "cm" #you can change to pixels etc
    )
    prc <- generate_prc_curve(x)
    plot(prc$curve)
    dev.off()
  }
}

