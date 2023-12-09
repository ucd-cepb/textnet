# textNet

textNet is a set of tools that uses part-of-speech tagging and dependency parsing to generate semantic networks from text data.

To be used in the main project repo: 
https://github.com/ucd-cepb/textNet

# Installation

Clone this repo and install with `devtools` from within project directory

```
devtools::install()
```

or using devtools::install_github():

```
devtools::install_github('ucd-cepb/textNet')
```
# Overview

The package has one core function, textnet_extract(). 

There are three optional pre-processing functions included that perform specific tasks: pdf_clean(), a wrapper of pdftools that converts pdf data to text; parse_text(), a wrapper of spacyr that conducts dependency parsing and named entity recognition; and find_acronyms(), which detects acronyms defined parenthetically in a string.

There are two optional post-processing functions: disambiguate(), which can merge and separate nodes according to user input, top_features(), which summarizes the most common lemmas and node names, and export_to_network(), which generates a network object using the network or igraph package and a collection of network-level summary statistics. 
There are also several helper functions used internally for tasks such as crawling the dependency tree of a sentence and concatenating entities.

To demo the package, try:
```{r}
library(textNet)
data("sample_20p")
extract_result <- textnet_extract(x = sample_20p,cl = 4,return_to_memory = T)
head(extract_result)
```

# Working on this package

Make changes to the code, then run `devtools::load_all()` and test them.
To update documentation and NAMESPACE file `devtools::document()`.
To reinstall the package: `devtools::install()`	

# Contact

Tyler Scott tascott at ucdavis dot edu

Elise Zufall ezufall at ucdavis dot edu

