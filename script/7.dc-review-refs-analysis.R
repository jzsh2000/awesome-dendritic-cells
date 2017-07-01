library(tidyverse)
library(magrittr)
library(widyr)
library(igraph)

refs <- read_tsv('output/dc-review-refs.txt',
                 col_names = c('pmc_id', 'pubmed_id'),
                 col_types = 'cc') %>%
    fill(pmc_id)

top_pubmed <- refs %>%
    count(pubmed_id) %>%
    arrange(desc(n), as.numeric(pubmed_id)) %>%
    slice(1:100) %T>%
    write_csv('output/dc-review-refs.count.csv')

# ===== find co-occurrence cliques
pubmed_cooccur = refs %>%
    pairwise_count(pubmed_id, pmc_id, sort = TRUE, upper = FALSE)
pubmed_cooccur_filtered = pubmed_cooccur %>%
    filter(n >= 20)
pubmed_cooccur_graph = graph_from_edgelist(
    as.matrix(pubmed_cooccur_filtered %>%
                  select(-n)), directed = FALSE)

pubmed_cooccur_cliques = max_cliques(pubmed_cooccur_graph, min = 3)
write_lines(map_chr(pubmed_cooccur_cliques,
                    ~paste(names(.x)[order(as.numeric(names(.x)))],
                           collapse = '-')),
            path = 'output/dc-review-refs.cliques.txt')
