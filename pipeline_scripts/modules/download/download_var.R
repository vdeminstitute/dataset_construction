# !/usr/bin/env Rscript

# Download a pre-specified variable

suppressMessages(library(dplyr))
suppressMessages(library(vbase))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Functions 
# ------------------------------------------------------------------------------

main <- function(TASK_NAME, qtable, db_data) {

    # Subset and get the variables we want
    qtable <- qtable[with(qtable, name == TASK_NAME & !is.na(name)), ]
    stopifnot(nrow(qtable) == 1)
    rating <- qtable$rating
    a_data <- qtable$a_data
    qid <- qtable$question_id

    if (!(any(rating | a_data)) | (rating & a_data)) {
        stop("Which table to download from?")
    }

    info("Downloading data for " %^% TASK_NAME)    
    if (rating) {
		df <- DBI::dbGetQuery(db_data,
	        paste0("SELECT * FROM rating WHERE question_id = ", qid, ";")) %>%
            mutate(id_a_data = NA_integer_)
    }
    if (a_data) {
        df <- DBI::dbGetQuery(db_data,
	        paste0("SELECT * FROM a_data WHERE question_id = ", qid, ";")) %>%
            mutate(rating_id = NA_integer_) %>%
			rename(id_a_data = id)
    }

    if (nrow(df) == 0) {
        stop("There is no data for this variable! " %^% TASK_NAME)
    }
    
    # Sort output
    df %<>%
        arrange(id_a_data, rating_id) %>%
        mutate(id = seq(from = 1, to = nrow(.), by = 1)) %>%
        arrange(question_id, country_id, historical_date, coder_id) %>%
        mutate(year = to_year(historical_date)) %>%
        select(id, rating_id, id_a_data, country_id, question_id, coder_id,
            code, confidence, historical_date, text_answer, timestamp, date_added,
            year) %>%
        left_join(qtable[, c("question_id", "name", "class")],
            by = "question_id")
    stopifnot(!anyNA(df$class))
    stopifnot(!anyNA(df$name))
    subDf <- df %>% select(question_id, rating_id, id_a_data, id)
    df %<>%
        select(-name, -class, -rating_id, -id_a_data) %>%
        as.data.frame()
    return(list(df = df, ids = subDf))
}

# Run script
# ------------------------------------------------------------------------------
# Database and globals
db <- pg_connect(Sys.getenv("PIPE_DB"))
db_data <- pg_connect(Sys.getenv("DOWNLOAD_DB"))
get_globals()

# Imports
qtable <- load_qtable()

# Run
collectedInputs <- named_list(TASK_NAME, qtable, db_data)
setNames(do.call(main, collectedInputs), c(TASK_NAME, "ids")) %>%
    write_file(., OUTFILE, dir_create = TRUE)
info("Done downloading " %^% TASK_NAME)
lock_task(db = db)
