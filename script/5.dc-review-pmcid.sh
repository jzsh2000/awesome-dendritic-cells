#!/usr/bin/env bash

cat dc-review.txt \
    | grep '^PMC ' \
    | cut -c7- \
    > dc-review.pmcid