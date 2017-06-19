library(tidyverse)
library(stringr)
library(lubridate)

medline <- data_frame(text = read_lines('dc-review.txt')) %>%
    filter(text != '') %>%
    mutate(major_line = map_lgl(text, ~str_detect(.x, '....-'))) %>%
    mutate(major_line_id = cumsum(major_line)) %>%
    group_by(major_line_id) %>%
    nest() %>%
    mutate(text_combined = map_chr(data, ~paste(str_replace(.x$text, '^  *', ''),
                                                collapse = ' '))) %>%
    select(text = text_combined) %>%
    mutate(record = str_extract(text, '(?<=^PMID- )[0-9]*'),
           field = str_trim(str_sub(text, end = 4)),
           value = str_sub(text, start = 7)) %>%
    select(-text) %>%
    fill(record, .direction = 'down') %>%
    filter(field %in% c('TI', 'FAU', 'AD', 'TA', 'JT', 'JID', 'EDAT'))

journal_df <- medline %>%
    filter(field %in% c('TA', 'JT', 'JID')) %>%
    spread(field, value) %>%
    group_by(JID, TA, JT) %>%
    count(record, sort = TRUE) %>%
    ungroup() %>%
    select(-record) %>%
    unique()

# find corresponding author
# if no email addresses provided: last author
# if all authors have email addressed provided: last author
# else: the last author has email address
medline_author = medline %>%
    filter(field %in% c('FAU', 'AD')) %>%
    group_by(record) %>%
    mutate(author_idx = cumsum(str_detect(field, 'FAU')),
           email = str_extract(value, '[^ ]+@[^ ]+'),
           contain_email = str_detect(value, '\\w@\\w')) %>%
    mutate(email = str_replace(email, '\\.$', '')) %>%
    filter(!(field == "AD" & !contain_email)) %>%
    # when an author has multiple email addresses, use the first one
    filter(!(field == "AD" & lag(field == "AD"))) %>%
    mutate(value = if_else(field == 'AD', email, value)) %>%
    select(-c(email, contain_email)) %>%
    nest() %>%
    rename(author_list = data) %>%
    mutate(author_list = map(author_list, ~spread(.x, field, value))) %>%
    mutate(corresponding = map_chr(author_list, function(x) {
        if (!("AD" %in% colnames(x))) {
            return(rev(paste0(x$FAU, '|-'))[1])
        } else if (sum(!is.na(x$AD)) == nrow(x)) {
            return(rev(paste(x$FAU, x$AD, sep = '|'))[1])
        } else {
            x = x %>% filter(!is.na(AD))
            return(rev(paste(x$FAU, x$AD, sep = '|'))[1])
        }
    })) %>%
    separate(corresponding, c('name', 'address'), sep = '\\|')

medline.wide = medline %>%
    filter(field %in% c('TI', 'JID', 'EDAT')) %>%
    spread(field, value) %>%
    left_join(read_tsv("dc-review.citedin",
                       col_names = c('record', 'citedin'),
                       col_types = 'ci'),
              by = 'record') %>%
    mutate(EDAT = ymd(str_extract(EDAT, '^[^ ]*'))) %>%
    left_join(medline_author %>% select(-author_list),
              by = 'record')

author_pool = sort(unique(medline.wide$name))
medline_df = medline.wide %>%
    mutate(name_completion = map_chr(name, function(name) {
        if (is.na(name)) return(NA_character_)
        name_part = str_split(name, ' ')[[1]]
        # there is a comma (,) after family name
        family_name_span = which(str_detect(name_part, ','))

        if (all(str_length(name_part) > 1)) {
            return(str_subset(author_pool, regex(name, ignore.case = TRUE))[1])
        } else {
            # find true author name:
            # `Liu, Yong-Jun` and `Liu, Y J` are the same author
            # `Steinman, R M` and `Steinman, Ralph M` are the same author
            query_pattern = str_trim(
                paste(paste(name_part[seq(family_name_span)],
                            collapse = ' '),
                      paste(sapply(name_part[-seq(family_name_span)],
                                   function(part) {
                                       ifelse(str_length(part) > 1,
                                              paste0(part, ' '),
                                              paste0(part, '[-a-z ]*'))
                                   }), collapse = ''),
                      collapse = ' ')
                )
            # print(query_pattern)
            search_res = str_subset(author_pool, regex(query_pattern))
            return(search_res[which.max(str_length(search_res))])
        }
    })) %>%
    # colnames of `journal_df` : "JID" "TA"  "JT"  "n"
    left_join(journal_df, by = 'JID') %>%
    select(record, EDAT, JT, TI, name, name_completion, address, citedin) %>%
    arrange(EDAT) %>%
    replace_na(list(name = '-', name_completion = '-', address = '-')) %>%
    rename(date = EDAT, journal_title = JT, title = TI)

medline_df %>% write_csv('output/dc-review.csv')

medline_df %>%
    group_by(name_completion) %>%
    summarise(total_reviews = n(),
              total_citedin = sum(citedin),
              max_citedin = max(citedin),
              max_citedin_pmid = record[which.max(citedin)],
              max_citedin_year = as.character(year(date)[which.max(citedin)]),
              year_start = as.character(min(year(date))),
              year_last = as.character(max(year(date)))
              ) %>%
    arrange(desc(total_reviews)) %>%
    rename(name = name_completion) %>%
    mutate(name = map_chr(name, function(x) {
        x_part = as.vector(str_split_fixed(x, ', ', 2))
        paste(rev(x_part), collapse = ' ')
        })) %>%
    write_csv('output/dc-review.author.csv')

medline_df %>%
    group_by(journal_title) %>%
    summarise(total_reviews = n(),
              total_citedin = sum(citedin),
              max_citedin = max(citedin),
              max_citedin_pmid = record[which.max(citedin)],
              max_citedin_year = as.character(year(date)[which.max(citedin)]),
              year_start = as.character(min(year(date))),
              year_last = as.character(max(year(date)))
              ) %>%
    arrange(desc(total_reviews)) %>%
    write_csv('output/dc-review.journal.csv')
