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

## Import control set data & build objects
pos_set_id <- "all_positive"
positive_set_ris <- read.delim(file = paste0("control_sets/processed/", pos_set_id, ".tsv"), stringsAsFactors=FALSE, header = T)[, c("tf_bnum", "gene_bnum")]
positive_set_tfs <- unique(positive_set_ris$tf_bnum)
positive_set <- cset(set(ris = positive_set_ris, tfs = positive_set_tfs), id = pos_set_id, type = "positive")

neg_set_id <- "all_negative"
negative_set_ris <- read.delim(file = paste0("control_sets/processed/", neg_set_id, ".tsv"), stringsAsFactors=FALSE, header = T)[, c("tf_bnum", "gene_bnum")]
negative_set_tfs <- unique(negative_set_ris$tf_bnum)
negative_set <- cset(set(ris = negative_set_ris, tfs = negative_set_tfs), id = neg_set_id, type = "negative")

# ## build artificial prediction set
# data_90_10 <- rbind.data.frame(dplyr::sample_n(positive_set_ris, 900, replace = F), dplyr::sample_n(negative_set_ris, 100, replace = F))
# data_90_10 <- cbind.data.frame(data_90_10, score1 = runif(1000), score2 = rnorm(1000))
# data_90_10 <- data_90_10 %>% select(tf_bnum, gene_bnum, score1, score2)
#
# predicted_set_ris <- data_90_10[, c("tf_bnum", "gene_bnum")]
# predicted_set_tfs <- unique(data_90_10$tf_bnum)
#
# ## to be moved in the constructor ?? rethink the score management should be on single data frame > problem with subset by tfs
# predicted_set_scores <- list()
# if (ncol(data_90_10) > 2)  {
# 	scores <- setdiff(colnames(data_90_10), c("tf_bnum", "gene_bnum"))
# 	for (s in scores)
# 	{
# 		predicted_set_scores[[s]] <- data_90_10[, s]
# 	}
# }
# predicted_set <- pset(set(ris = predicted_set_ris, tfs = predicted_set_tfs), scores = predicted_set_scores)

## read and format prediction set
pred_set_id <- "physical_network_tf_gene" #physical_network_tf_gene
data_tf_tu <- read.delim(file = paste0("predictions/", pred_set_id, ".tsv"), stringsAsFactors=FALSE, header = T)
data_tf_tu <- data_tf_tu %>% dplyr::select(-tf_symbol, -gene_symbol, -normalized_height, -distanceBSToStart)#, -auc_PHmax_interval, -auc_distMean_interval, -PHsum, -auc_distClosest_interval, -auc_distMean_interval)

predicted_set_ris <- data_tf_tu %>% dplyr::select(tf_bnum, gene_bnum)
predicted_set_tfs <- unique(predicted_set_ris$tf_bnum)

## to be moved in the constructor ?? rethink the score management should be on single data frame > problem with subset by tfs
predicted_set_scores <- list()
if (ncol(data_tf_tu) > 2)  {
	scores <- setdiff(colnames(data_tf_tu), c("tf_bnum", "gene_bnum"))
	for (s in scores)
	{
		predicted_set_scores[[s]] <- data_tf_tu[, s]
	}
}

predicted_set <- pset(set(ris = predicted_set_ris, tfs = predicted_set_tfs), scores = predicted_set_scores)

## TF selection - to be moved into a function
test_tfs <- sort(intersect(intersect(get_tfs(predicted_set), get_tfs(positive_set)), get_tfs(negative_set)))
tfs_n <- length(test_tfs)

filtered_positive_set <- subset_by_tfs(positive_set, test_tfs)
filtered_negative_set <- subset_by_tfs(negative_set, test_tfs)
filtered_predicted_set <- subset_by_tfs(predicted_set, test_tfs) ##

filtered_predicted_ris <- paste0(filtered_predicted_set@ris$tf_bnum, "_", filtered_predicted_set@ris$gene_bnum)

filtered_predicted_set_scores <- data_tf_tu %>%
	dplyr::mutate(ri=paste0(tf_bnum, "_", gene_bnum)) %>%
	dplyr::filter(ri %in% filtered_predicted_ris) %>%
	dplyr::select(-ri)

predicted_set_scores_filtered <- list()
if (ncol(filtered_predicted_set_scores) > 2)  {
	scores <- setdiff(colnames(filtered_predicted_set_scores), c("tf_bnum", "gene_bnum"))
	for (s in scores)
	{
		predicted_set_scores_filtered[[s]] <- filtered_predicted_set_scores[, s]
	}
}

filtered_predicted_set@scores <- predicted_set_scores_filtered

discarded_tfs <- setdiff(get_tfs(predicted_set), get_tfs(filtered_predicted_set))
discarded_tfs_n <- get_tfs_n(predicted_set) - get_tfs_n(filtered_predicted_set)

# identical(get_tfs(filtered_negative_set), get_tfs(filtered_positive_set))
# identical(get_tfs(filtered_negative_set), get_tfs(filtered_predicted_set))
# identical(get_tfs(filtered_positive_set), get_tfs(filtered_predicted_set))


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
# my_eval <- evalset(positive_set, negative_set, predicted_set)
my_eval <- evalset(filtered_positive_set, filtered_negative_set, filtered_predicted_set)

# for (s in names(my_eval@pred_set@scores)) {
# 	generate_roc_curve(my_eval, s)
# }

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
# generate_roc_curve(my_eval)

## nb add id to pred set and remove here
rmarkdown::render("report.Rmd", params = list(evalset = my_eval, predictions = predicted_set, id=pos_set_id), output_file = paste0(pred_set_id, "-", pos_set_id, "-", neg_set_id, ".html"))

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
