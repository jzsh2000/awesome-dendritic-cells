library(tidyverse)
library(stringr)
library(lubridate)

medline <- data_frame(text = read_lines('dc-review.text.txt')) %>%
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
# else: the author has email address
medline_author = medline %>%
    filter(field %in% c('FAU', 'AD')) %>%
    group_by(record) %>%
    mutate(author_idx = cumsum(str_detect(field, 'FAU')),
           email = str_extract(value, '[^ ]+@[^ ]+'),
           contain_email = str_detect(value, '\\w@\\w')) %>%
    mutate(email = str_replace(email, '\\.$', '')) %>%
    filter(!(field == "AD" & !contain_email)) %>%
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
