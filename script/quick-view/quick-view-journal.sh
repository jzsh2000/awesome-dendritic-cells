#!/usr/bin/env bash

grep -e '^JT' dc-review.txt \
    | cut -c7- \
    | sort \
    | uniq -c \
    | sort -k1,1nr \
    | head 
