# !/usr/bin/env Rscript
#
#
# Download all vignettes and save them in a single file

# class has been adjusted in qtable to have all vignettes

suppressMessages(library(vutils))
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))

# get task_id and question_name
db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
OUTFILE <- create_outfile(id = Sys.getenv("TASK_ID"),
                          ROOT = Sys.getenv("ROOT_DIR"),
                          db = db)
db_data <- pg_connect(Sys.getenv("DOWNLOAD_DB"))

# refs
qtable <-
    read_file(file.path(ROOT, "refs", "question_table.rds")) %>%
    filter(class == "V")

# Assertions
stopifnot(all(qtable$rating))
qids <- qtable$question_id %>%
    paste0(collapse = ", ")

# download vignettes
info("Downloading vignettes, please wait....")
query <- "SELECT * FROM rating WHERE question_id in (" %^% qids %^% ");"


df_download <- DBI::dbGetQuery(db_data, query) %>%
    mutate(default_id = NA_integer_)

v_miss <- setdiff(qtable$question_id, df_download$question_id)
info("Data missing for vignettes: ")
print(qtable %>% filter(question_id %in% v_miss) %$% name)

df <-
    df_download %>%
    arrange(question_id, default_id, rating_id) %>%
    # We want a higher default_id / rating_id to correspond to higher id
    mutate(id = seq_along(rating_id)) %>%
    arrange(question_id, country_id, historical_date, coder_id) %>%
    mutate(year = to_year(historical_date)) %>%
    select(id, country_id, question_id, coder_id,
        code, confidence, historical_date, text_answer, timestamp, date_added,
        party_id, year)

# Assertions
df %<>% left_join_(qtable[, c("question_id", "name", "class")],
                   by = "question_id")
stopifnot(!anyNA(df$class))
stopifnot(!anyNA(df$name))
df %<>% select(-name, -class)

varname <- "vignettes"
out <- list()
out[[varname]] <- df

# write file
###
info("Done downloading vignettes")
write_file(out, OUTFILE, dir_create = T)
update_task_status(db = db)
lock_task(db = db)
