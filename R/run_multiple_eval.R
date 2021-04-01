#' @name run_multiple_eval
#' @title Run the evaluation of a prediction set.
#' @description Format the input prediction set, filters it, and generates an evaluation report.
#' @author Claire Rioualen
#' @param testfile The path to a prediction file.
#' @param neg_set_id Optional, the ID of the negative set to be used from c("all_negative", "test_neg", "negative_1_1", "negative_2_1", "negative_2_2")
#' @param pos_set_id Optional, the ID of the positive set to be used from c("all_positive", "test_pos", "positive_2_1", "positive_2_2", "positive_2_3")
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
	## Summary

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

	## Histogram

	long <- tidyr::gather(summary_df, "variable", "value", c("Sensitivity", "Specificity", "Precision"))
	#long <- long %>% dplyr::mutate(group=ifelse(grepl("expression", Set_ID), "expression", ifelse(grepl("physical", Set_ID), "physical", "control")))
	long$variable <- factor(long$variable, levels = c("Sensitivity", "Specificity", "Precision"))

	ggplot(long, aes(x = Set_ID, y = value)) +
		geom_bar(stat = "identity", fill = "royalblue") +
		#geom_col(aes(fill = group)) +
		scale_fill_manual(guide=guide_legend(reverse=T)) +
		theme(axis.text.y = element_text(size=10), axis.title.x = element_blank(), legend.title = element_blank(), strip.text.x = element_text(size = 10), strip.text.y = element_text(size = 10), axis.title.y = element_blank(), legend.position="none", legend.text = element_text(size = 11), legend.direction="vertical", axis.text.x = element_text(angle = 90, hjust=1, size=10)) +
		facet_grid(variable ~ .)
	# png(paste0(summary_dir, "/histogram.png"))
	# print(p)
	# dev.off()
	ggsave(paste0(summary_dir, "/histogram.png"), device="png", width = 6, height = 4)

	# Cairo::Cairo(
	# 	20, #length
	# 	20, #width
	# 	file = paste0(summary_dir, "/histogram.png"),
	# 	type = "png", #tiff
	# 	bg = "transparent", #white or transparent depending on your requirement
	# 	dpi = 200,
	# 	units = "cm" #you can change to pixels etc
	# )
	# ggplot(long, aes(x = Set_ID, y = value)) +
	# 	geom_bar(stat = "identity", fill = "royalblue") +
	# 	#geom_col(aes(fill = group)) +
	# 	scale_fill_manual(guide=guide_legend(reverse=T)) +
	# 	theme(axis.text.y = element_text(size=10), axis.title.x = element_blank(), legend.title = element_blank(), strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14), axis.title.y = element_blank(), legend.position="none", legend.text = element_text(size = 11), legend.direction="vertical", axis.text.x = element_text(angle = 90, hjust=1, size=12)) +
	# 	facet_grid(variable ~ .)
	# dev.off()

	## Venn to update ++++

	positive_set <- read.delim(file = system.file("control_sets", paste0(pos_set_id, ".tsv"), package = "NetworkEval"), header=T, stringsAsFactors=FALSE)
	positive_set <- positive_set %>% dplyr::mutate(pair= paste0(tf_bnum, "_", gene_bnum))
	positive_set <- positive_set %>% dplyr::arrange(tf_bnum, gene_bnum)
	positive_set <- positive_set %>% dplyr::distinct(pair, .keep_all = TRUE)

	negative_set <- read.delim(file = system.file("control_sets", paste0(neg_set_id, ".tsv"), package = "NetworkEval"), header=T, stringsAsFactors=FALSE)
	negative_set <- negative_set %>% dplyr::mutate(pair= paste0(tf_bnum, "_", gene_bnum))
	negative_set <- negative_set %>% dplyr::arrange(tf_bnum, gene_bnum)
	negative_set <- negative_set %>% dplyr::distinct(pair, .keep_all = TRUE)

	prediction_set <- data.frame(matrix(ncol=2), stringsAsFactors = F)
	colnames(prediction_set) <- c("tf_bnum", "gene_bnum")
	for(s in set_ids) {
		ris <- read.delim(file = paste0(predictions_dir, "/", s, ".tsv"), header=T, stringsAsFactors=FALSE)[c("tf_bnum", "gene_bnum")]
		prediction_set <- rbind.data.frame(prediction_set, ris)
	}
	prediction_set <- prediction_set %>% dplyr::mutate(pair= paste0(tf_bnum, "_", gene_bnum))
	prediction_set <- prediction_set %>% dplyr::arrange(tf_bnum, gene_bnum)
	prediction_set <- prediction_set %>% dplyr::distinct(pair, .keep_all = TRUE)


	data <- list(Predictions=prediction_set$pair, Positive=positive_set$pair)
	png(paste0(summary_dir, "/venn1.png"),   width= 3,height= 3,units= "in",res= 150)
	plot(euler(data, shape = "ellipse"), fills =  c("lightblue", "lightcoral"), quantities=T)
	dev.off()

	data_bis <- list(Predictions=prediction_set$pair, Positive=positive_set$pair, Negative=negative_set$pair)
	plot(euler(data_bis, shape = "circle"), fills =  c("lightblue", "lightcoral", "white"))
		## Upset

	upset(fromList(data_bis), nsets = length(data_bis), number.angles = 30, point.size = 3, line.size = 2,
				mainbar.y.label = "Sets Intersections", sets.x.label = "Set IDs",
				text.scale = c(2, 2, 2, 1, 1.5, 1.2), order.by='freq')
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
