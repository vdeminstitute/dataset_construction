# !/usr/bin/env Rscript
#
# Download a prespecified variable

suppressMessages(library(vutils))
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "download_var")

main <- function(VARNAME, qtable, db_data) {
    qtable %<>% filter(name == VARNAME)
    qid <- qtable$question_id

    if (!(any(qtable$rating | qtable$default_rating)) |
        (qtable$rating & qtable$default_rating)) {
        stop("Which table to download?")
    }

    info("Downloading data for " %^% VARNAME)    
    if (qtable$rating) {
	# using DBI's function directly is faster and I guess when we have over 300 vars, it makes sense to use it instead of {dplyr}
		df <- DBI::dbGetQuery(db_data,
	        paste0("SELECT * FROM rating WHERE question_id = ", qid, ";")) %>%
            mutate(default_id = NA_integer_)
    }
    if (qtable$default_rating) {
        df <- DBI::dbGetQuery(db_data,
	        paste0("SELECT * FROM default_rating WHERE question_id = ", qid, ";")) %>%
            mutate(confidence = NA_integer_,
                   rating_id = NA_integer_,
                   coder_id = NA_integer_,
                   date_added = NA_character_,
                   party_id = NA_integer_)
    }

    if (nrow(df) == 0)
        stop("There is no data for this variable! " %^% VARNAME)
    

    df %<>%
        arrange(default_id, rating_id) %>%
        # We want a higher default_id / rating_id to correspond to higher id
        # It means we recieved the observation at a later point in time.
        mutate(id = seq(from = 1, to = nrow(.), by = 1)) %>%
        arrange(question_id, country_id, historical_date, coder_id) %>%
        mutate(year = to_year(historical_date)) %>%
        select(id, rating_id, default_id, country_id, question_id, coder_id,
            code, confidence, historical_date, text_answer, timestamp, date_added,
            party_id, year) %>%
        left_join_(qtable[, c("question_id", "name", "class")],
                   by = "question_id")
    stopifnot(!anyNA(df$class))
    stopifnot(!anyNA(df$name))
    subDf <- df %>% select(question_id, rating_id, default_id, id)
    df %<>% select(-name, -class, -rating_id, -default_id) %>% untibble
    return(list(df = df, ids = subDf))
}

# Run script
# Global variables
get_globals()
# Imports
qtable <- load_qtable() 
db_data <- pg_connect(Sys.getenv("DOWNLOAD_DB"))
# Run
collectedInputs <- named_list(VARNAME, qtable, db_data)
setNames(do.call(main, collectedInputs), c(VARNAME, "ids")) %>%
    write_file(., OUTFILE, dir_create = T)
info("Done downloading " %^% VARNAME)
update_task_status(db = db)
lock_task(db = db)
