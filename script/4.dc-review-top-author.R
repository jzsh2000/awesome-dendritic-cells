library(tidyverse)

read_csv('output/dc-review.author.csv',
                        col_types = 'ciiiciii') %>%
    arrange(desc(max_citedin)) %>%
    filter(total_reviews >= 3,
           max_citedin >= 100,
           total_citedin / total_reviews >= 50)
