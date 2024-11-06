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

To demo the package, see vignettes/textNet_vignette_2024.pdf for a reproducible example that transforms raw text data into event networks.

# Working on this package

Make changes to the code, then run `devtools::load_all()` and test them.
To update documentation and NAMESPACE file `devtools::document()`.
To reinstall the package: `devtools::install()`	

# Contact

Elise Zufall ezufall at ucdavis dot edu

Tyler Scott tascott at ucdavis dot edu

