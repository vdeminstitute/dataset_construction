#!/usr/bin/env Rscript


options(warn = 2)
library(dplyr)
suppressMessages(library(vutils))




create_duration_var <- function(df) {
    info("Creating v2regdur")

	
    stopifnot(any(!grepl("-  \\d", df$text_answer)))

    df_out <- df %>%
        select(country_id, historical_date, text_answer, year) %>%
        mutate(reg_start = gsub("(.*\\()(\\d{2}/\\d{2}/\\d{4} )(.*)", "\\2", x = text_answer),
        reg_end = gsub("(.*- )(\\d{2}/\\d{2}/\\d{4})(\\))", "\\2", x = text_answer)) %>%
        mutate(across(starts_with("reg_"), ~as.Date(.x, format = "%d/%m/%Y"))) %>%
        mutate(code = ifelse(historical_date <= reg_end | is.na(reg_end), historical_date - reg_start, 0))

    stopifnot(all(
        !is.na(df_out$code)),
        !any(df_out$code < 0)
    )
    stopifnot({
		(df_out[["code"]] == 0) == 
			(df_out$historical_date == df_out$reg_start)
	})
    stopifnot(nrow(df) == nrow(df_out))

    df_out %<>% 
		select(country_id, historical_date, code, year) %>%
		arrange(country_id, historical_date)
    return(df_out)
}


create_regid_var <- function(df) {
    info("Creating v2regidnr")
    df_out <- df %>%
        arrange(country_id, historical_date) %>%
        distinct(country_id, text_answer) %>%
        group_by(country_id) %>%
        mutate(idcode = 1:n()) %>%
        ungroup() %>%
        mutate(idcode = ifelse(idcode < 10, paste0(0, idcode), as.character(idcode)),
            idcode = as.numeric(paste0(country_id, idcode))) %>%
        inner_join(., df, by = c("country_id", "text_answer")) %>%
        select(country_id, historical_date, code = idcode, year) %>%
		arrange(country_id, historical_date)
    
    stopifnot(any(!is.na(df_out$code)))
    stopifnot(nrow(df) == nrow(df_out))

    return(df_out)
}

merge_qtable <- function(df, qtable, VARNAME) {
    var_qdf <- filter(qtable, name == VARNAME) %>% select(question_id)
    return(bind_cols(var_qdf, df))
}

# Main
main <- function(df, qtable, VARNAME) {

    stopifnot(all(!is.na(df[["text_answer"]])))
    stopifnot(VARNAME %in% c("v2regdur", "v2regidnr"))
    df %<>% arrange(country_id, historical_date)
    df_proc <- if (VARNAME == "v2regdur") create_duration_var(df) else create_regid_var(df)
    df_out <- merge_qtable(df_proc, qtable, VARNAME)

    return(df_out)
}


# Run script
if (no_test()) {
    # Global variables
    get_globals()
    
    # Imports
    qtable <- load_qtable()
    df <- find_dep_files(TASK_ID, DB)[["v2reginfo"]][["v2reginfo"]]

    # Run
    collectedInputs <- named_list(df, qtable, VARNAME)
    setNames(list(do.call(main, collectedInputs)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Created " %^% VARNAME)

} # There're no tests, so no else statement for the moment
update_task_status(db = DB)
