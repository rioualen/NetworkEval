#' @name run_eval
#' @title Run the evaluation of a predictoin set.
#' @description Format the input prediction set, filtters it, and generates an evaluation report.
#' @author Claire Rioualen
#' @param testfile The path to a prediction file.
#' @param neg_set_id Optional, the ID of the negative set to be used from c("all_negative", "test_neg")
#' @param pos_set_id Optional, the ID of the positive set to be used from c("all_positive", "test_pos")
#'
#' @import rmarkdown
#' @export
run_eval <- function(testfile, neg_set_id = "all_negative", pos_set_id = "all_positive") {
	negative_set <- format_cset(id = neg_set_id, type = "negative")
	positive_set <- format_cset(id = pos_set_id, type = "positive")
	predicted_set <- format_pset(file = testfile)

	## filter tfs that are present in all 3 sets
	filtered_sets <- select_tfs(negative_set, positive_set, predicted_set)

	## make evalset object
	my_eval <- evalset(pos_set = filtered_sets$pos_set, neg_set = filtered_sets$neg_set, pred_set = filtered_sets$pred_set,
										 out_tfs = filtered_sets$out_tfs, out_ris = filtered_sets$out_ris)

	## run evaluation
	rmarkdown::render("report.Rmd", params = list(evalset = my_eval), output_file = paste0("reports/", predicted_set@id, ".html"))
}
