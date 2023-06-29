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

The package has once core function, custom_entity_extract2(). There are a series of helper functions included that perform specific tasks, such as crawwling the dependency tree of a sentence and concatenating entities.

To demo the package, try:
```{r}
library(textNet)
data("sample_10p")
extract_result <- custom_entity_extract2(x = sample_10p,cl = 5,return_to_memory = T)
head(extract_result)
```

# Working on this package

Make changes to the code, then run `devtools::load_all()` and test them.
To update documentation and NAMESPACE file `devtools::document()`.
To reinstall the package: `devtools::install()`	

# Contact

Tyler Scott tscott1 at ucdavis dot edu

Elise Zufall ezufall at ucdavis dot edu

