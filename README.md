# NetworkEval

The goal of NetworkEval is to evaluate the acccuracy of a set of interactions transcription factor-target gene in *Escherichia coli* K12, based on a set of custom control files.

## Installation

``` r
remotes::install_github("rioualen/NetworkEval")
```

 
## Example

The input interaction file must be formatted as follows:

* It is a tab-delimited file

* It has at least two columns containing TF bnumbers and target genes bnumbers, and named "tf_bnum" and "gene_bnum".

* Gene names con be converted to bnumbers using the [EcoliGenes](https://github.com/rioualen/EcoliGenes) packages (hopefully the conversion should be integrated in the evaluation process).

* Optionally, the file can contain one or more score columns, which name should be prefixed by "score_". This allows to generate ROC curves.


``` r
library(NetworkEval)

## Run a quick test set with small control sets
testfile="test_sets/test_90_10_scores.tsv"
run_eval(testfile, pos_set_id = "test_pos", neg_set_id = "test_neg")

## Run your own evaluation using all of the controls sets
run_eval("path/to/your/file")
```

