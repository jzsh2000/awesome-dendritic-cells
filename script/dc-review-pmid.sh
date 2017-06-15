#!/usr/bin/env bash

esearch -db pubmed -query '"dendritic cells"[mesh] and "review"[pt]' \
    | efetch -format medline \
    | tee dc-review.txt \
    | grep '^PMID' \
    | cut -c7- \
    > dc-review.pmid
