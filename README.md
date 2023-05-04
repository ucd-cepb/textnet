# citationSearch

citationSearch is an R package that provides an interface for creating a
searchable Solr index from a dataframe of records, as well as searching against
that index with a dataframe of parsed citations. It was designed to allow
searching citations against record metadata that is not contained in common
citation indexes, e.g government grey literature.

To be used in the main project repo: 
https://github.com/govscienceuseR

# Setup

Have Solr instance running.

Install Solr from https://solr.apache.org  
Start an instance in cloud mode: `solr start -c`

Navigate to http://localhost:8983/solr/ to see if solr is running.

# Installation

Clone this repo and install with `devtools` from within project directory

```
devtools::install()
```

# Overview

This package expects the user to provide two datasets:
1. a dataframe of the references you have found in documents
2. a dataframe of ground-truth references you want to match against.

## 1. Record metadata

The record metadata should be a dataframe with the following columns:
| Field | Type | Description
|---| ---| ---
| title |  String | Title of the paper 
| authors |  String | Concatenated list of authors (e.g "Hileman, Jacob; Bastos, Marco T. A; Lubell, Mark")
| year |  Int | 4 digit year (between 1800 and 2025)
| publisher |  String | Name of publisher
| journal_title |  String | Name of journal
| doi |  String | DOI
| source |  String | Helpful name you have given for the collection
| miscid |  String | A unique id field 

See an example in the package provided data:
`citationSearch::wos_column_corrected`

Any missing entries should be set to R's `NA` value.

## 2. Citation data

Citations should be stored as a dataframe with the following columns:
| Field | Type | Description
|---| ---| ---
| title |  String | Title of the paper 
| authors |  String | Concatenated list of authors (e.g "Hileman, Jacob; Bastos, Marco T. A; Lubell, Mark")
| year |  Int | 4 digit year (between 1800 and 2025)
| publisher |  String | Name of publisher
| journal_title |  String | Name of journal
| doi |  String | DOI

## Searching

### Create an index from your records (do this once)

```{r}
records = citationSearch::wos_column_corrected # sample data for demo purposes
index_records(records, collection_name="WOS_demo")
```

### Search against the records

To do a single search using Solr's query syntax:
```{r}
results = search_collection(q = "authors:tyler AND title:collaboration", 
			    collection_name = "WOS_demo", 
			    topn=5)
```

To search using a dataframe of citation data described above.
You can do something like:
```{r}
citations = citationSearch::parsed_anystyle
queries = create_queries(citations)

results = list()
count = 1
for (q in queries) {
    res = search_collection(q, collection_name="WOS_demo")
    res$id = count
    res$q = q
    results[[count]] = res
    count = count + 1
}
results_df = do.call(dplyr::bind_rows, results)
```

# Working on this package

Make changes to the code, then run `devtools::load_all()` and test them.
To update documentation and NAMESPACE file `devtools::document()`.
To reinstall the package: `devtools::install()`	

# Contact

Arthur Koehl avkoehl at ucdavis dot edu

Tyler Scott tscott1 at ucdavis dot edu

