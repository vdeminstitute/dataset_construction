#!/usr/bin/env Rscript

# ==========================================================================
# C-variable v2regsupgroups (aggr by mean) and
# C-variable v2regsupgroupssize (mm) get cleaned by A-variable v2regint
# ==========================================================================

library(vutils)
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "regint_cleaning")


#
# Functions
# --------------------------------------------------------------------------
check_cleaning_rule <- function(codebook, VARNAME) {
	rule <- codebook %>%
			select(question_id, tag, cleaning) %>%
    	filter(tag == VARNAME) %$% cleaning
	stopifnot(length(rule) == 1)
	stopifnot(!is.na(rule))
	stopifnot(rule != "")
	stopifnot(grepl("missing", rule))
	stopifnot(grepl("regint", rule))
	return(NULL)
}

merge_regint <- function(df, v2regint, utable) {
	v2regint %<>% select(country_id, historical_date, regint = code)

	df %<>%
	    full_join(select(v2regint, regint, country_id, historical_date),
              by = c("country_id", "historical_date")) %>%
    	interpolate_vdem_col("regint", utable)
	df <- df[!is.na(df$code) | !is.na(df$text_answer), ]
	return(df)
}

filter_dirty <- function(df) {
	df %>% filter(regint == 0) %>% return(.)
}

filter_clean <- function(df, dirt) {
	df %>% filter(!id %in% dirt$id) %>% select(-regint) %>% return(.)
}

main <- function(codebook, country_unit, df, VARNAME, v2regint) {
	check_cleaning_rule(codebook, VARNAME)
	df <- merge_regint(df, v2regint, country_unit)
	dirt <- filter_dirty(df)
	dirtylist[["v2regint_cleaning"]] <<- dirt
	return(filter_clean(df, dirt))
}


# Run script
if (no_test()) {
    # Global variables
    get_globals()
    dirtylist <- list()

    # Imports
    codebook <-  load_codebook()
    country_unit <- load_country_unit()
    objs <- find_dep_files(TASK_ID, DB)
    df <- objs[[VARNAME]][[VARNAME]]
    v2regint <- objs[["v2regint"]][["v2regint"]]

    # Run
    collectedInputs <- named_list(codebook, country_unit, df, VARNAME, v2regint)
    setNames(list(do.call(main, collectedInputs), dirtylist), 
        c(VARNAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = T)    
    info("Done with regint_cleaning...")
} else {
    testthat::test_file("~/proj/mm-prep/tests/downstream_cleaning2/test_regint_cleaning.R")
}
update_task_status(db = DB)