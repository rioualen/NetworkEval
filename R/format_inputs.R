#' @name format_cset
#' @title Generate a cset from the input parameters.
#' @description Reads the control file and builds the object.
#' @author Claire Rioualen
#'
#' @param id The id of a control set
#' @param type Type from c("negative", "positive")
#' @param dir Optional, by default internal directory "control_sets"
#'
#' @export
format_cset <- function(id, type, dir = "control_sets") {
	cfile <- system.file(dir, paste0(id, ".tsv"), package = "NetworkEval")
	cset_ris <- read.delim(file = cfile, stringsAsFactors=FALSE, header = T)[, c("tf_bnum", "gene_bnum")]
	cset_tfs <- unique(cset_ris$tf_bnum)
	cset <- cset(set(ris = cset_ris, tfs = cset_tfs), id = id, type = type)
	cset
}

#' @name format_pset
#' @title Generate a pset from the input file.
#' @description Format the input prediction set and builds the object.
#' @author Claire Rioualen
#'
#' @param file The input prediction file, a 2+ column file with colnames: c("tf_bnum", "gene_bnum", ...)
#' @param tfs TF list, a list of bnumbers
#'
#' @import EcoliGenes
#' @export
format_pset <- function(file, tfs="") {
	pred_data <- read.delim(file = file, stringsAsFactors=FALSE, header = T)
	# pred_scores <- pred_data %>% dplyr::select(grep("score_", colnames(df)))
	predicted_set_ris <- pred_data #%>% dplyr::select(tf_bnum, gene_bnum)
	if (!tfs=="") {
		predicted_set_tfs <- read.delim(file = tfs, stringsAsFactors=FALSE, header = F)$V1
	} else {
		predicted_set_tfs <- unique(predicted_set_ris$tf_bnum) ## should be added the possibility of providing tf list separately
	}
	predicted_set_scores <- colnames(pred_data)[grep("score_", colnames(pred_data))]##
	predicted_set_id <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(file))
	predicted_set <- pset(set(ris = predicted_set_ris, tfs = predicted_set_tfs), scores = predicted_set_scores, id = predicted_set_id)
}

#' @name select_tfs
#' @title Filter the 3 sets' TFs before evaluation
#' @description Get the list of common TFs between all 3 sets and select RIs accordingly
#' @author Claire Rioualen
#'
#' @param neg_set A cset object of type "negative"
#' @param pos_set A cset objecct of type "positive"
#' @param pred_set A pset object
#'
#' @import dplyr
#' @export
select_tfs <- function(neg_set, pos_set, pred_set) {
	test_tfs <- intersect(get_tfs_set(pred_set), get_tfs_set(pos_set))
	test_tfs <- intersect(test_tfs, get_tfs_set(neg_set))

	filtered_positive_set <- subset_by_tfs(pos_set, test_tfs)
	filtered_negative_set <- subset_by_tfs(neg_set, test_tfs)
	filtered_predicted_set <- subset_by_tfs(pred_set, test_tfs)

	tfs_filtered_out <- setdiff(get_tfs_set(pred_set), test_tfs)
	ris_filtered_out <- pred_set@ris %>% dplyr::filter(tf_bnum %in% tfs_filtered_out)

	list(pos_set = filtered_positive_set, neg_set = filtered_negative_set, pred_set = filtered_predicted_set, out_tfs = tfs_filtered_out, out_ris = ris_filtered_out)
}

