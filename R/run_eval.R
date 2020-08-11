run_eval <- function(testfile, neg_set_id = "all_negative", pos_set_id = "all_positive") {
	negative_set <- format_cset(id = neg_set_id, type = "negative")
	positive_set <- format_cset(id = pos_set_id, type = "positive")
	predicted_set <- format_pset(file = testfile)

	# print(dim(negative_set@ris))
	# print(dim(positive_set@ris))
	# print(dim(predicted_set@ris))
	## select tf

	filtered_sets <- select_tfs(negative_set, positive_set, predicted_set)
	# discarded_tfs <- setdiff(get_tfs(predicted_set), get_tfs(filtered_predicted_set))
	# discarded_tfs_n <- get_tfs_n(predicted_set) - get_tfs_n(filtered_predicted_set)

	# print(dim(filtered_sets$neg_set@ris))
	# print(dim(filtered_sets$pos_set@ris))
	# print(dim(filtered_sets$pred_set@ris))

	## make evalset object
	my_eval <- evalset(pos_set = filtered_sets$pos_set, neg_set = filtered_sets$neg_set, pred_set = filtered_sets$pred_set,
										 out_tfs = filtered_sets$out_tfs, out_ris = filtered_sets$out_ris)

	# print(dim(my_eval@neg_set@ris))
	# print(dim(my_eval@pos_set@ris))
	# print(dim(my_eval@pred_set@ris))

	## run evaluation

	rmarkdown::render("report.Rmd", params = list(evalset = my_eval), output_file = paste0("reports/", predicted_set@id, ".html"))

}



# tfs_temp <- intersect(get_tfs(positive_set), get_tfs(negative_set))
# temp_pos <- positive_set@ris %>% dplyr::filter(tf_bnum %in% tfs_temp) %>% dplyr::sample_n(900, replace = F)
# temp_neg <- negative_set@ris %>% dplyr::filter(tf_bnum %in% tfs_temp) %>% dplyr::sample_n(100, replace = F)
# test_temp <- rbind.data.frame(temp_pos, temp_neg)
#
# write.table(test_temp, file = "../NetworkEval/test_sets/test_90_10_simple.tsv", quote = F, row.names=F, col.names=T, sep = "\t")
#

# score_1 <- rnorm(1000)
# score_2 <- runif(1000)
# test_temp <- cbind.data.frame(test_temp, score_1, score_2)
#
# write.table(test_temp, file = "../NetworkEval/test_sets/test_90_10_scores.tsv", quote = F, row.names=F, col.names=T, sep = "\t")
# #
# pred_tfs <- unique(test_temp$tf_bnum)
#
# temp_test_pos <- setdiff(positive_set@ris, temp_pos)
# temp_test_pos <- temp_test_pos %>% dplyr::filter(tf_bnum %in% pred_tfs) %>% dplyr::sample_n(1100, replace = F)
# temp_test_pos <- rbind.data.frame(temp_pos, temp_test_pos)
# write.table(temp_test_pos, file = "../NetworkEval/control_sets/test_pos.tsv", quote = F, row.names=F, col.names=T, sep = "\t")
#
# temp_test_neg <- setdiff(negative_set@ris, temp_neg)
# temp_test_neg <- temp_test_neg %>% dplyr::filter(tf_bnum %in% pred_tfs) %>% dplyr::sample_n(1900, replace = F)
# temp_test_neg <- rbind.data.frame(temp_neg, temp_test_neg)
# write.table(temp_test_neg, file = "../NetworkEval/control_sets/test_neg.tsv", quote = F, row.names=F, col.names=T, sep = "\t")
#

