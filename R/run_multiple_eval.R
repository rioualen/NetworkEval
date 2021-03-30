#' @name run_multiple_eval
#' @title Run the evaluation of a prediction set.
#' @description Format the input prediction set, filters it, and generates an evaluation report.
#' @author Claire Rioualen
#' @param testfile The path to a prediction file.
#' @param neg_set_id Optional, the ID of the negative set to be used from c("all_negative", "test_neg")
#' @param pos_set_id Optional, the ID of the positive set to be used from c("all_positive", "test_pos")
#'
#' @import rmarkdown
#' @export
run_multiple_eval <- function(predictions_dir, results_dir = "results", category = "test", tfs="", neg_set_id = "all_negative", pos_set_id = "all_positive") {

  ## Print prediction directory
	## Print input sets

	## Run eval for all (loop)

	## Generate additional output files
		## Venn
		## Upset
		## Categories


	## produce report // right now it only works locally
	# print(getwd())
	# rmarkdown::render("R/test.r")
	# rmarkdown::render("Summary.Rmd", output_file= paste0(results_dir, "/my_file.html"))
	rmarkdown::render("R/test.r", output_file= paste0("../", results_dir, "/my_file.html"))
	# rmarkdown::render("report.Rmd", params = list(evalset = my_eval), output_file = paste0("reports/", predicted_set@id, ".html"))
}
