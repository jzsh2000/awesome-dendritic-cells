#!/usr/bin/env bash

for pmcid in $(cat dc-review.pmcid)
do
    esearch -db pmc -query $pmcid \
        | elink -db pmc -target pubmed -name pmc_refs_pubmed \
        | efetch -format uid \
        | paste <(echo $pmcid) - \
        >> output/dc-review-refs.txt
done
