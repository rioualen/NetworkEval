#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

## usage
# Rscript --vanilla eval_report.R test_sets/pos_1 test_sets/neg_1 test_sets/pred_90_10

devtools::load_all()

if (length(args)!=3) {
	stop("Please supply 3 parameters: positive set folder, negative set folder, prediction set folder", call.=FALSE)
}
pos_set_dir <- args[1]
pos_set_id  <- basename(pos_set_dir)

neg_set_dir <- args[2]
neg_set_id  <- basename(neg_set_dir)

pred_set_dir <- args[3]
pred_set_id  <- basename(pred_set_dir)

## Import set data & build objects
positive_set_ris <- read.delim(file = paste0("test_sets/", pos_set_id, "/ris.tsv"), stringsAsFactors=FALSE, header = T)
positive_set_tfs <- read.delim(file = paste0("test_sets/", pos_set_id, "/tfs.tsv"), stringsAsFactors=FALSE, header = T)$tf_symbol
positive_set <- cset(set(ris = positive_set_ris, tfs = positive_set_tfs), id = pos_set_id, type = "positive")

negative_set_ris <- read.delim(file = paste0("test_sets/", neg_set_id, "/ris.tsv"), stringsAsFactors=FALSE, header = T)
negative_set_tfs <- read.delim(file = paste0("test_sets/", neg_set_id, "/tfs.tsv"), stringsAsFactors=FALSE, header = T)$tf_symbol
negative_set <- cset(set(ris = negative_set_ris, tfs = negative_set_tfs), id = neg_set_id, type = "negative")


predicted_set_ris <- read.delim(file = paste0("test_sets/", pred_set_id, "/ris.tsv"), stringsAsFactors=FALSE, header = T)
predicted_set_tfs <- read.delim(file = paste0("test_sets/", pred_set_id, "/tfs.tsv"), stringsAsFactors=FALSE, header = T)$tf_symbol
predicted_set_scores <- data.frame(runif(1000))
predicted_set <- pset(set(ris = predicted_set_ris, tfs = predicted_set_tfs), scores = predicted_set_scores)

# ## Testing set methods  -> should be moved to example section of each method
# summarize(positive_set)
# summarize(negative_set)
# summarize(predicted_set)
# get_tfs(positive_set)
# intersect_by_ris(predicted_set, positive_set)
# get_ris(predicted_set)
# get_ris_n(predicted_set)
# get_tfs(predicted_set)
# get_tfs_n(predicted_set)

## Build evalset object and get stats and confusion matrix
my_eval <- evalset(positive_set, negative_set, predicted_set)

# ## Testing evalset methods -> should be moved to example section of each method
# summarize_stats(my_eval)
# get_tfs_eval(my_eval)
# get_universe(my_eval)
# get_tfs(get_universe(my_eval))
# sensitivity(my_eval)
# specificity(my_eval)
# precision(my_eval)
# fdr(my_eval)
# true_pos(my_eval)
# false_neg(my_eval)
# actual_pos(my_eval)
# false_pos(my_eval)
# true_neg(my_eval)
# actual_neg(my_eval)
# pred_pos(my_eval)
# pred_neg(my_eval)
# total_pop(my_eval)
# outer_set(my_eval)
# generate_confusion_matrix(my_eval)
# get_sets(my_eval)
# generate_venn_diagram(my_eval, style=3, universe=FALSE)

rmarkdown::render("report.Rmd", params = list(evalset = my_eval), output_file = paste0(pred_set_id, "-", pos_set_id, "-", neg_set_id, ".html"))

### stuff to check

# show functions?
# summarize function for evalset?
# inheritance between evalset and set and method names #??
# set ids en input?
# pb params de fault de venn
## should add one accessor per set get_neg_set, get_pos_set

# tous ces tests: examples in functions

## TODO check why not all ris in the eval set are in the eval universe -- id/names issues?
# get_ris_n(union_by_ris(union_by_ris(positive_set, negative_set), predicted_set))
# get_ris_n(intersect_by_ris(union_by_ris(union_by_ris(positive_set, negative_set), predicted_set), get_universe(my_eval)))
