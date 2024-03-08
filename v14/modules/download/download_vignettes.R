# !/usr/bin/env Rscript
#
# Download all vignettes and save them in a single file
# -- Class has been adjusted in qtable to have all vignettes as V

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# get task_id and question_name
db <- pg_connect()
db_data <- pg_connect(Sys.getenv("DOWNLOAD_DB"))
get_globals()

# read question table
qtable <- load_qtable()
qtable <- qtable[qtable$class == "V", ]

# Assertions
stopifnot(`vignettes should be fetched from rating table` = all(qtable$rating))
stopifnot(`non-V entries found` = all(qtable$class == "V"))
stopifnot(`entries are not unique` = length(unique(qtable$question_id)) == nrow(qtable))

# Download vignettes
info(sprintf("Downloading %d vignettes.", nrow(qtable)))
query <- selectFromWhereSQL(
    table = "rating",
    condition = sprintf("question_id in (%s)", toString(qtable$question_id)))
df_download <- DBI::dbGetQuery(db_data, query)
df_download[["id_a_data"]] <- NA_integer_

# Which vignettes are missing?
# -- missing if you are in qtable but not in df_download: Q \nin V
v_miss <- setdiff(qtable$question_id, df_download$question_id)
info("Data missing for vignettes: ")
print(qtable$name[qtable$question_id %in% v_miss])

# Transform vignettes
df <-
    df_download %>%
    arrange(question_id, id_a_data, rating_id) %>%
    # We want a higher id_a_data / rating_id to correspond to higher id
    mutate(id = seq_along(rating_id)) %>%
    arrange(question_id, country_id, historical_date, coder_id) %>%
    mutate(year = to_year(historical_date)) %>%
    select(id, country_id, question_id, coder_id,
        code, confidence, historical_date, text_answer, timestamp, date_added,
        year) %>% 
    as.data.frame()

# Merge for results
res <- merge(
    x = df,
    y = qtable[, c("question_id", "name", "class")],
    by = "question_id",
    all.x = TRUE)
# Assertions
stopifnot(nrow(df) == nrow(res))
stopifnot(!anyNA(res$class))
stopifnot(!anyNA(res$name))
# Remove columns as we do not need them
res[["class"]] <- NULL
res[["name"]] <- NULL
# Set new properties
out <- list()
out[["vignettes"]] <- res

# Write outfile
write_file(out, OUTFILE, dir_create = TRUE)
lock_task(db = db)
