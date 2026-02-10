# textNet

textNet is a set of tools in the R language that uses part-of-speech tagging and dependency parsing to generate semantic networks from text data. It is compatible with Universal Dependencies and has been tested on English-language text data.

To be used in the main project repo: 
https://github.com/ucd-cepb/textNet

# Overview
Network extraction from documents has typically required manual coding.
Furthermore, existing network extraction methods that use co-occurrence
leave a vast amount of data on the table, namely, the rich edge
attribute data and directionality of each verb phrase defining the
particular relationship between two entities, and the respective roles
of the entity nodes involved in that verb phrase. We present an R
package, *textNet*, designed to enable directed, multiplex, multimodal
network extraction from text documents through syntactic dependency
parsing, in a replicable, automated fashion for collections of
arbitrarily long documents. The *textNet* package facilitates the
automated analysis and comparison of many documents, based on their
respective network characteristics. Its flexibility allows for any
desired entity categories, such as organizations, geopolitical entities,
dates, or custom-defined categories, to be preserved.

See vignettes/paper.pdf for an overview of the package functionality and potential use cases.  

To demo the package, see vignettes/textNet_vignette_2025.pdf for a reproducible example that transforms raw text data into event networks.

# Installation

Install the `pak` package if you don't already have it:

```
install.packages("pak")
```

Then install textNet from GitHub:

```
pak::pak('ucd-cepb/textNet')
```

Alternatively, clone this repo and install from within the project directory:

```
pak::local_install()
```

# Suggested packages
The primary function, textnet_extract(), can be used without the use of spaCy, if the user prefers to import compatible data from a separate tool. A wrapper of the spacyr package is included for convenience, to conduct preprocessing in-house. Use of this functionality requires installation of the reticulate and spacyr packages, as seen below. Use of the spacyr wrapper also requires installing spaCy and a language model; the 'en_core_web_lg' model is recommended (see below). For assistance, please see the spacyr documentation page: https://spacyr.quanteda.io/reference/spacy_install.html

```
pak::pak(c("reticulate", "spacyr"))
library(spacyr)
spacy_install()
spacy_download_langmodel('en_core_web_lg')
```

# Guidelines for Contributors
See CONTRIBUTING.md for guidelines for working on this package.

# Contact

Elise Zufall ezufall at ucdavis dot edu

Tyler Scott tascott at ucdavis dot edu

