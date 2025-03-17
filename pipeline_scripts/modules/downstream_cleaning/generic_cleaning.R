#!/usr/bin/env Rscript

# ==========================================================================
# Clean C or A variable by an A variable
# This is a generic cleaning script that can set values to missing. The func-
# tion is conditioned upon a TASK_NAME, a cleaning rule (from the codebook). 
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------

# Get cleaning rule 
# -- one variable can have mulitple rules
check_cleaning_rule <- function(codebook, cleaningVar) {
    
    cleaning <- codebook[["cleaning"]]    
    stopifnot(`rule from codebook is of length zero` = length(cleaning) > 0)

    # The split-token is hard-coded and it needs to be used.
	ruleVec <- unlist(strsplit(x = cleaning, split = ";"))
	# Create rule
    rule <- ruleVec[grepl(cleaningVar, ruleVec)]
    stopifnot(`after splitting the rule, found some delimiters` =
        !grepl(pattern = "[;:/]", x = rule))

    # |Rule| needs to be 1 as we only work with one cleaning step at a time.
    stopifnot(`no cleaning rule could be found` = length(rule) == 1)
    stopifnot(`we only set values to missing`= grepl("missing", rule))
    stopifnot(`we only remove based on numeric values for cleaning_var` = 
        grepl("\\d\\.?$", rule))
    stopifnot(!is.na(rule))
    stopifnot(rule != "")
	stopifnot(grepl(cleaningVar, rule, fixed = TRUE))

    return(rule)
}

add_cleaning_bool <- function(df, rule) {

    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`rule needs have class character` = is.character(rule))

    # get the value of the cleaning variable
    # -- grab what comes after "is", trim, if there is a period remove it
    ruleShort <- gsub("[.]$", "", trimws(gsub("^.* is(.*)$", "\\1", rule), which = "both"))
    stopifnot(`ruleShort is not a character` = is.character(ruleShort))

    # Separate the conditions
    # -- can only handle two conditions at the moment
	if (grepl("or", ruleShort)) {
        ruleVec <- as.numeric(c(
            # Separate the values
            gsub("^(\\d+).*?$", "\\1", ruleShort), 
			gsub("^.*?(\\d+)$", "\\1", ruleShort)))

	} else {
		ruleVec <- as.numeric(gsub("^.*?(\\d+)$", "\\1", ruleShort))
	}

    # Note that the cleaning_var has been added by another function    
	bool <- df[["cleaning_var"]] %in% ruleVec & !is.na(df[["cleaning_var"]])

	stopifnot(`bool cannot have missingness` = !anyNA(bool))
    stopifnot(`bool has wrong length` = length(bool) == nrow(df))
	df[["bool"]] <- bool

	return(df)
}


main <- function(utable, codebook, df, cleaningVarDf, TASK_NAME, qtable, cleaningVar) {

    # Assert and retrieve cleaning rule
    rule <- check_cleaning_rule(codebook, cleaningVar)
    info(sprintf("Running %s_cleaning for %s with rule: [%s]",
        cleaningVar, TASK_NAME, rule))

	# Set cleaning_var_df: cleaning_var is a generic name for the cleaning variable
    cleaningVarDf <- cleaningVarDf[, c("code", "country_id", "historical_date")]
    names(cleaningVarDf) <- c("cleaning_var", "country_id", "historical_date")
    

    # For C variables
    # -- we need to interpolate at coder-level for C data
	if (qtable$class == "C") {

        candData <-
            vutils::add_cleaning_var(df, cleaningVarDf) |>
            vutils::interpolate_components(df=_, cols="cleaning_var",
                utable, coder_level = TRUE) |>
            vutils::drop_extra_rows(df=_) |> 
            add_cleaning_bool(df=_, rule=rule)
        outdf <-
            clean_observations(
                df = candData, bool = candData[["bool"]],
                function_name = sprintf("%s_cleaning", cleaningVar),
                description = rule)
        
        rm(candData)
        outdf[["bool"]] <- NULL
        outdf[["cleaning_var"]] <- NULL

    # For all non-C data
	} else {
		# Class other than C
        candData <-
            vutils::add_cleaning_var(df, cleaningVarDf) |>
            vutils::interpolate_cleaning_var(df=_, "cleaning_var", utable) |>
            vutils::drop_extra_rows(df=_) |> 
            add_cleaning_bool(df=_, rule=rule)
        outdf <-
            clean_observations(df = candData, bool = candData[["bool"]],
            function_name = sprintf("%s_cleaning", cleaningVar),
            description = rule)

        rm(candData)
        outdf[["bool"]] <- NULL
        outdf[["cleaning_var"]] <- NULL
	}

    outdf <- vbase::organiseRows(outdf, country_id, historical_date)
    stopifnot(`out_df is not a data.frame` = is.data.frame(outdf))

	return(outdf)
}

# Run script
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    utable <- load_country_unit()
	qtable <- load_qtable()
    qtable <- qtable[qtable$name == TASK_NAME, ]
    codebook <- load_codebook()
    codebook <- codebook[codebook$tag == TASK_NAME, ]
    objs <- find_dep_files(TASK_ID, db)
    df <- objs[[TASK_NAME]][[TASK_NAME]]
	cleaningVar <- names(objs)[!names(objs) %in% TASK_NAME]
    cleaningVarDf <- objs[[cleaningVar]][[cleaningVar]]

    stopifnot(nrow(codebook) == 1) 
    stopifnot(nrow(qtable) == 1)
    stopifnot(!is.na(qtable[["class"]]))
    
    # Run
    collectedInputs <- named_list(utable, codebook, df, cleaningVarDf, TASK_NAME, qtable,
		cleaningVar)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("Done with generic_cleaning")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/downstream_cleaning/test_generic_cleaning.R")
}
