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

# combine cliques
pubmed_cooccur_cliques_matrix = sapply(pubmed_cooccur_cliques, function(x) {
    sapply(pubmed_cooccur_cliques, function(y) {
        length(intersect(names(x), names(y)))
        })
    }) >= 2
pubmed_cooccur_cliques_graph = graph_from_adjacency_matrix(
    pubmed_cooccur_cliques_matrix,
    mode = 'undirected'
)
pubmed_cooccur_cliques_clusters = clusters(pubmed_cooccur_cliques_graph)
pubmed_cooccur_cliques_clusters = groups(pubmed_cooccur_cliques_clusters)

pubmed_cooccur_cliques_new = sapply(pubmed_cooccur_cliques_clusters,
                                    function(x) {
                                        Reduce(union,
                                               lapply(pubmed_cooccur_cliques[x],
                                                      names))
                                    })
sapply(pubmed_cooccur_cliques_new,
       function(x) {
           paste(x[order(as.numeric(x))],
                 collapse = ' ')
       }) %>%
    write_lines(path = 'output/dc-review-refs.cliques.txt')
