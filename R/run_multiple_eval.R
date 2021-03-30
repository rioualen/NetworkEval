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

	## run evaluation of all sets if not already done
	pred_files <- list.files(predictions_dir, full.names = T)
	set_ids <- c()
	for(f in pred_files){
		set <- basename(tools::file_path_sans_ext(f))
		# print(set)
		set_ids <- c(set_ids, set)
		if(!dir.exists(paste0(results_dir, "/", set))) {
			NetworkEval::run_eval(f)
		}
	}

  ## Print prediction directory
	## Print input sets


	## Generate additional output files
	## summary

	Set_ID <- c()
	RIs_input <- c()
	TFs_input <- c()
	RIs_TFfiltered <- c()
	TFs_filtered <- c()
	RIs_RIfiltered <- c()
	# AUC <- c()
	Sensitivity <- c()
	Specificity <- c()
	Precision <- c()
	TP <- c()
	FP <- c()

	# set_ids <- list.files("results")
	for(set in set_ids) {
		confusion <- read.delim(file = paste0(results_dir, "/", set, "/", set, "_confusion_matrix.tsv"), header=T, stringsAsFactors=FALSE)
		in_ris <- read.delim(file = paste0(results_dir, "/", set, "/", set, "_in_stats.tsv"), header=T, stringsAsFactors=FALSE)
		out_ris <- read.delim(file = paste0(results_dir, "/", set, "/", set, "_out_stats.tsv"), header=T, stringsAsFactors=FALSE)
		stats <- read.delim(file = paste0(results_dir, "/", set, "/", set, "_statistics.tsv"), header=T, stringsAsFactors=FALSE)

		Set_ID <- c(Set_ID, set)
		RIs_input <- c(RIs_input, in_ris$RIs)
		TFs_input <- c(TFs_input, in_ris$TFs)
		RIs_TFfiltered <- c(RIs_TFfiltered, out_ris$RIs)
		TFs_filtered <- c(TFs_filtered, out_ris$TFs)
		RIs_RIfiltered <- c(RIs_RIfiltered, confusion$total_population[1])
		# AUC <- c()
		Sensitivity <- c(Sensitivity, stats$sensitivity)
		Specificity <- c(Specificity, stats$specificity)
		Precision <- c(Precision, stats$precision)
		TP <- c(TP, confusion$control_positive[1])
		FP <- c(FP, confusion$control_negative[1])
	}

	summary_df <- data.frame(Set_ID , RIs_input, TFs_input, RIs_TFfiltered, TFs_filtered, RIs_RIfiltered, Sensitivity, Specificity, Precision, TP, FP, stringsAsFactors=F)

	summary_dir <- paste0(results_dir, "/summary_report")
	if(!dir.exists(summary_dir)) {
		dir.create(summary_dir)
	}
	write.table(summary_df, file = paste0(summary_dir, "/summary_table.tsv"), col.names = T, row.names = F, quote = F, sep = "\t")
	#summary_df <- summary_df %>% dplyr::mutate(group=ifelse(grepl("expression", Set_ID), "expression", ifelse(grepl("physical", Set_ID), "physical", "control")))
		## Venn
		## Upset
		## Categories


	## produce report // right now it only works locally

	## report
	sumfile <- system.file("rmarkdown", "Summary.Rmd", package = "NetworkEval")
	file.copy(sumfile, ".", overwrite = T)

	rmarkdown::render("Summary.Rmd",
										params = list(predictions_dir = "predictions", results_dir = "results"),
										output_file = "my_report.html")
	# print(getwd())
	# rmarkdown::render("R/test.r")
	# rmarkdown::render("Summary.Rmd", output_file= paste0(results_dir, "/my_file.html"))
	#rmarkdown::render("R/test.r", output_file= paste0("../", results_dir, "/my_file.html"))
	# rmarkdown::render("report.Rmd", params = list(evalset = my_eval), output_file = paste0("reports/", predicted_set@id, ".html"))
}
