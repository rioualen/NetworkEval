#' Basic funtions to get and work with lists of genes and tfs.
#' This section should evolve once we have a proper standard: right now there are inconsistencies between RegulonDB and Ensembl gene annotations.
#' Eventually we want to have a proper function to filter/cconvert gene ids and names.
#'
#' We should also be able to convert tf names properly between RegulonDB and Zika, while giving particualr attention to dimeric names.
#'
#'
#' @title Get transcription factors from RegulonDB
#' @description Get list of transcription factor coding gene names from RegulonDB
#' @author Claire Rioualen
#' @param file A TFSet file downloaded from RegulonDB.
#' @return A character vector.
#' @export
get_regulondb_tfs <- function() {

  tfs_df <- read.delim("data/regulondb/TFSet.txt", header=F, sep="\t", comment.char = "#", stringsAsFactors = F)
  tfs <- unique(sort(tfs_df$V3))
  tfs <- tfs[!tfs %in% c("", "3'ETS<sup><i>leuZ</i></sup>")] ## should disappear once once we have a proper way to handle tfs

  tfs
}

#' @title Get genes from RegulonDB
#' @description Get genes from RegulonDB. Note: the RegulonDB files have to be previously downloaded (to be included in setup - maybe a makefile?).
#' @author Claire Rioualen
#' @return A character vector.
#' @export
get_regulondb_genes <- function() {

  genes_df <- read.delim("data/regulondb/GeneProductSet.txt", header=F, sep="\t", comment.char = "#", stringsAsFactors = F)
  genes <- unique(sort(genes_df$V2))
  genes <- genes[!genes %in% c("", "3'ETS<sup><i>leuZ</i></sup>")] ## should disappear once once we have a proper way to handle genes

  genes
}

#' @title Get genes from Ensembl
#' @description Get genes from Ensembl. Note: I custom-made the currently available file. Should be inddcluded in future makefile.
#' @author Claire Rioualen
#' @return A character vector.
#' @export
get_ensembl_genes <- function() {

  genes_df <- read.delim("data/ensembl/gene.tab", header=T, sep="\t", comment.char = "#", stringsAsFactors = F)
  genes <- unique(sort(genes_df$name))

  genes
}

#' @title Get first genes of TUs from RegulonDB
#' @description Get TU first genes from RegulonDB. Note: the RegulonDB files have to be previously downloaded (to be included in setup - maybe a makefile?).
#' @author Claire Rioualen
#' @return A character vector.
#' @export
get_regulondb_tu_first_gene <- function() {

  tus_df <- read.delim("data/regulondb/TUSet.txt", header=F, sep="\t", comment.char = "#", stringsAsFactors = F)
  genes <- c()
  for (i in 1:nrow(tus_df)) {
    genes <- as.character(tus_df$V4[i])
    first_gene <- strsplit(genes, split=",")[[1]][1]
    first_gene <- strsplit(first_gene, split="-")[[1]][1]
    genes <- c(genes, first_gene)
  }
  genes <- unique(sort(genes))
  genes
}

