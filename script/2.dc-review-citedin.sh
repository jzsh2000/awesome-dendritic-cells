#!/usr/bin/env bash

cat dc-review.pmid \
    | parallel --jobs 1 \
        echo {} ';' \
        elink -db pubmed -target pubmed -name pubmed_pubmed_citedin -id {} \
        '|' efetch -format uid \
        '|' wc -l \
    | paste - - \
    > dc-review.citedin
