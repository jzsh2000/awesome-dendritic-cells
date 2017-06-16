#!/usr/bin/env bash

grep -e '^FAU' -e '^JID' dc-review.txt \
    | grep -B1 '^JID' \
    | grep '^FAU' \
    | cut -c7- \
    | sort \
    | uniq -c \
    | sort -k1,1nr \
    | head 
