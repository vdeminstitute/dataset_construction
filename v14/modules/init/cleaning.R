#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Generic cleaning applied to variables and vignettes.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vbase))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))
options(dplyr.summarise.inform = FALSE)

#
# Functions
# --------------------------------------------------------------------------

# Rename name in country table to country_name 
rename_country_name <- function(df) {

    df %<>% rename(country_name = name)
    stopifnot(!c("name") %in% names(df))
    stopifnot(is.data.frame(df))

    return(df)
}

# Assert that the id column is unique, not NA, and numeric.
check_unique <- function(df) {
    
    stopifnot(
        `df lacks id` = !is.null(df[["id"]]),
        `rows are not unique` = length(unique(df[["id"]])) == nrow(df),
        `id contains NA` = !anyNA(df[["id"]]),
        `id needs to be numeric` = is.numeric(df[["id"]])
        )

    return(df)
}

# Do we have observations for the latest year?
check_newest_year <- function(df, TASK_NAME, qtable) {

	NEWEST_YEAR <- as.integer(Sys.getenv("NEWEST_YEAR"))
	stopifnot(`newest_year is missing`= !is.na(NEWEST_YEAR))
    info(sprintf("Checking if we have observations for the newest year: %d", NEWEST_YEAR))
    # Account for no_update variables
    no_update <- qtable$no_update[qtable$name == TASK_NAME]
	stopifnot(`This v2 variable has no data for the newest year!` = 
		!(
			grepl("^v2", TASK_NAME) &
			!any(df$year == NEWEST_YEAR) &
			!isTRUE(no_update) & 
			!TASK_NAME %in% c(
                "v2vignettes",
                "v2x_elecreg", "v2xex_elecreg", "v2xlg_elecreg",
                "v2ex_hosw")
		))
	return(df)
}

# Remove sequential observations: these are observations for backfill questions and backfill coders pre 2005
remove_sequential <- function(df, TASK_NAME, qtable, coder) {

	stopifnot(isTRUE(TASK_NAME %in% qtable$name) | TASK_NAME == "vignettes")
    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`coder has zero rows` = nrow(coder) > 0)

    if (TASK_NAME == "vignettes") {
        info("Sequential observations are not relevant for vignettes")
        return(df)
    }

    info("Removing sequential codings")
    # -- (1) Identify if a question contains sequential observations
    backfill_bool <- qtable$backfill_question[qtable$name == TASK_NAME]
    stopifnot(`backfill_question has missingness` = !is.na(backfill_bool))
    # If backfill_bool is FALSE, return df    
    if (!isTRUE(backfill_bool)) {
        return(df)
    }

    # -- (2) Identify if a coder has submitted sequential ratings
    df <- merge(
        x = df,
        y = coder[, c("coder_id", "backfill_coder")],
        by = "coder_id",
        all.x = TRUE)
    
    # -- bool is TRUE for sequential observations
	bool <- df$backfill_coder & df$year < 2005

    # Remove sequential observations
	res <- clean_observations(
        df = df,
        bool = bool,
        function_name = "remove_sequential", 
		description = "Remove sequential observations")
    res[["backfill_coder"]] <- NULL

    return(res)
}

# Recode country_ids in ratings or a_data
recoding_country_ids <- function(df) {

    stopifnot(nrow(df) > 0)
    info("Merging Chinas and Saudi Arabias")
    # Merge CHINA with China
    df$country_id[df$country_id == 213] <- 110
    # Merge Nejd with Saudi Arabia for historical merge
    df$country_id[df$country_id == 374] <- 197

    return(df)
}

# Merge with reference tables and check that questions and countries in our data match our reference tables
merge_reference_tables <- function(df, qtable, countries) {

    stopifnot(`df has zero rows` = nrow(df) > 0)
    
    qtableCols <-
        c("question_id", "name", "class", "disaggregated", "min", "code_col",
        "text_col", "question_type", "cb_section")
    qtable <- qtable[, qtableCols]
    countryCols <-
        c("country_id", "parent_country_id", "end_date", "codingstart_contemp",
        "codingstart_hist")
    countries <- countries[, countryCols]

    info("Merge reference tables")
    inRow <- nrow(df)
    df <- merge(
        x = df,
        y = qtable,
        by = "question_id",
        all.x = TRUE)
    stopifnot(`merge with reference tables changed the number of rows` =
        inRow == nrow(df))
    
    info("Assert that ratings exists only for countries in countries table")
    # -- this merge must happen after recode_country_ids()
    res <- merge(
        x = df,
        y = countries,
        by = "country_id",
        all.x = TRUE)
    stopifnot(`merge with reference tables changed the number of rows` =
        nrow(df) == nrow(res))

    # -- Check:
    stopifnot(!(res$class != "V" & !grepl("^v\\dzz", res$name) & is.na(res$country_id)))

    return(res)
}

# Remove observations outside official coding periods
clean_codingstart_outside <- function(df) {
    
    stopifnot(
        `There are 0 rows in the input` = nrow(df) > 0,
        `The ratings are not unique` = length(unique(df$id)) == nrow(df),
        `The df does not have name in it` =
            c("name") %in% names(df))
    
    info("Clean observations outside official coding periods...")
    
    bool <- df$class != "V" &
        !grepl("^v\\dzz", df$name) &
        !is.na(df$country_id) &
        is.na(df$codingstart_contemp) &
        is.na(df$codingstart_hist)

    df <- clean_observations(df, bool, 
		"clean_codingstart_outside",
		"Remove observations before codingstart")

    return(df)
}

# Recode and remove coders
recode_and_remove_coders <- function(df, coder) {

    stopifnot(`df has zero rows` = 
        nrow(df) > 0L)
    stopifnot(`coder has zero rows` = 
        nrow(coder) > 0L)
    info("Merging historical coders who also coded contemporary data...")
    df %<>%
        mutate(coder_id = case_when(
            coder_id == 4248 ~ 636,
            coder_id == 4251 ~ 860,
            coder_id == 4252 ~ 1392,
            coder_id == 4387 ~ 419,
            coder_id == 4396 ~ 1674,
            coder_id == 4403 ~ 2914,
            coder_id == 4260 ~ 2979,
            coder_id == 4400 ~ 3074,
            coder_id == 5266 ~ 4080,
            coder_id == 5224 ~ 4527,
            coder_id == 5232 ~ 4541,
            coder_id == 5257 ~ 5525,
            TRUE ~ as.numeric(coder_id)))

	# Test coder: We want their A* and A data, but not C-data.
	if (df$class[1] == "C") {
		info("Removing test coders for C-data...")
        test_coders <- coder$coder_id[coder$test_coder]
		bool <- df$coder_id %in% test_coders
		df <- clean_observations(df, bool, "recode_and_remove_coders",
			"Remove test coders")
	}

    # Removing ratings from two coders:
    bool <-
        df$coder_id == 831 & !is.na(df$coder_id) & df$country_id == 40 &
        grepl("^wsm", df$cb_section)
	df <- clean_observations(df, bool, "recode_and_remove_coders",
			"Remove Kenya coder")	

	bool <- df$coder_id == 522 & !is.na(df$coder_id)    
	df <- clean_observations(df, bool, "recode_and_remove_coders",
			"Remove fraudulent coder")

    return(df)
}


clean_hist_overlap <- function(df) {
    info("Recoding historical countries to their successor in the overlap period.")
    # Change historical countries to successor countries, as historical coders
    # also code the period 1900-1920 for the successor. The purpose is to decrease
    # sparsity in the ratings matrix for the IRT model.

    stopifnot(`df has zero rows` = nrow(df) > 0L)
    stopifnot(`df is lacking class column` = !is.null(df[["class"]]))
    stopifnot(`class has missingness` = !anyNA(df[["class"]]))

    # bool needs as long as df
    bool <- df$class == "C" & !is.na(df$class) &
            !is.na(df$parent_country_id) &
            !is.na(df$end_date) &
            to_year(df$historical_date) > df$end_date

    stopifnot(`bool cannot have missingness` = !anyNA(bool))
    stopifnot(`bool is not of same length as df` = length(bool) == nrow(df))

    df[["country_id"]] <- ifelse(bool, df$parent_country_id, df$country_id)

    # Non-C data for historical countries from 1900-1920 will be cleaned out with
    # utable cleaning.

    return(df)
}


clean_text_answer <- function(df) {
    stopifnot(`df has zero rows` = nrow(df) > 0)

    info("Removing new line characters in text_answer")
    df %<>% mutate(text_answer = rm_newline(text_answer))

    info("Setting empty strings in text_answer to NA")
    is.na(df$text_answer) <- df$text_answer == ""
    
    return(df)
}

# Set observations to missing if they have responses in the wrong columns
# -- e.g a code_col question that has non-missingness in text_answer 
wrong_column <- function(df) {

    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`df is lacking code_col` = !is.null(df[["code_col"]]))
    stopifnot(`df is lacking text_col` = !is.null(df[["text_col"]]))

    info("Setting code/text_answer to missing if it is in wrong column")

    if (df[["code_col"]][1]) {
        df %>% 
            filter(code_col) %>%
            filter(!is.na(text_answer)) %>%
            group_by(name, question_type) %>%
            summarize(n = n()) %>%
            arrange(desc(n)) %>% {
                if (nrow(.) > 0) {
                    info("Code questions with responses in text_answer:")
                    print(x = .)
                } else {
                    info("No code question with response in text_answer") 
                }
            }
        }

    if (df[["text_col"]][1]) {
        df %>%
            filter(text_col) %>%
            filter(!is.na(code)) %>%
            group_by(name, question_type) %>%
            summarize(n = n()) %>%
            arrange(desc(n)) %>% {
                if (nrow(.) > 0) {
                    info("Text_answer questions with response in code column:")
                    print(x = .)
                } else {
                    info("No text_answer question with response in code")
                }
            }
        }

    # Setting code or text_col to missing
    is.na(df$text_answer) <- df$code_col & !is.na(df$text_answer)
    is.na(df$code) <- df$text_col & !is.na(df$code)

    return(df)
}

# Remove observation that has missing values in either code or text_answer
remove_missing_observations <- function(df, TASK_NAME) {

    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`TASK_NAME is missing` = TASK_NAME != "")


	if (TASK_NAME == "vignettes") {
		value_col <- "code"
	} else {
        if (df$code_col[1]) {
            value_col <- "code"
        } else {
            value_col <- "text_answer"
        }
	}

    info("Cleaning observations that has NA for code or text_answer")    
	# Cleaning condition
	bool <- is.na(df[[value_col]])

	df <- clean_observations(df, bool, "remove_missing_observations",
			"Remove missing values")

    stopifnot(`The filter did not remove all missing observations` =
        !anyNA(df[[value_col]]))

    return(df)
}

# Change and transfer "yes/no" text answer to 1/0 code for v2juflow
juflow_recoding <- function(df, TASK_NAME) {

    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`question_id is not unique` =
        (1 == length(unique(df[["question_id"]]))) | TASK_NAME == "vignettes")

    if (TASK_NAME == "vignettes") {
        return(df)
    }

    # v2juflow has correct question_type and answer should be in code column
    juflow_qid <- 2798
    
    if (unique(df$question_id) == juflow_qid) {

        info("For v2juflow, change yes/no to 1/0")
        juflow <- df$text_answer[df$question_id == juflow_qid]

        df[df$question_id == juflow_qid, "code"] <-
            as.numeric(juflow == "yes")

        is.na(df$text_answer) <- df$question_id == juflow_qid
    }

    return(df)
}

# Recode v3elsnlsff and v3clrgunev to correct direction
hist_elsnlsff_clrgunev_recoding <- function(df) {

    # COMMENT: Why not change in postgres?
    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`code is of class character` = !is.character(df[["code"]]))
    
    # If not elsnlsff or clgrunev, return early
    if (!df$question_id[1] %in% c(2464, 2505)) {
        return(df)
    }
    
    if (all(is.na(df[["code"]]))) {
        return(df)
    }
    
    # Conditional switch:
    if (any(df[["code"]] %in% c(0, 2))) {
        info("Recoding of v3clrgunev/v3elsnlsff from 0 to 2 or 2 to 0.")
        df %<>% mutate(code = case_when(
            (timestamp > "2017-06-15 14:53:31.308404") & (code == 0) ~ 2,
            (timestamp > "2017-06-15 14:53:31.308404") & (code == 2) ~ 0,
            TRUE ~ code))

        return(df)
    }

    return(df)
}

# Recode v2jupack code 4 to 3
jupack_recoding <- function(df) {

    stopifnot(`df has zero rows` = nrow(df) > 0)

    info("v2jupack convert category 4 to 3")
    df$code[df$question_id == 580 & df$code == 4] <- 3

    return(df)
}

# Create data_type for historical or contemporary data
# -- used for merging historical and contemporary data
flag_historical_observations <- function (df) {

    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`name has missingness`= !any(is.na(df[["name"]])))

    # Keep the data_type column for later when v2 and v3 are merged.
    df[["data_type"]] <- ifelse(grepl("^v3", df[["name"]]), "historical", "normal")

    return(df)
}

main <- function(TASK_NAME, df, qtable, country, coder) {

    stopifnot(`TASK_NAME is empty` = TASK_NAME != "")
    stopifnot(`TASK_NAME is NA` = !is.na(TASK_NAME))

    country <- rename_country_name(country)

	outdf <-
        check_unique(df) %>% 
		check_newest_year(., TASK_NAME, qtable) %>%
		remove_sequential(., TASK_NAME, qtable, coder) %>%  
        recoding_country_ids(.) %>%
        merge_reference_tables(., qtable, country) %>%
        clean_codingstart_outside(.) %>% 
        recode_and_remove_coders(., coder) %>% 
        clean_hist_overlap(.) %>% 
        juflow_recoding(., TASK_NAME) %>%
        clean_text_answer(.) %>%
        wrong_column(.) %>%
        remove_missing_observations(., TASK_NAME) %>%  
        hist_elsnlsff_clrgunev_recoding(.) %>%
        jupack_recoding(.) %>%
        flag_historical_observations(.)  %>% 
        as.data.frame()
    
    stopifnot(`result is a tibble` = !"tbl_df" %in% class(outdf))
    stopifnot(`result has zero rows` = nrow(outdf) > 0)
    stopifnot(`result has more rows that input` = nrow(outdf) <= nrow(df))

    outdf <- outdf[, c("id", "question_id", "country_id", "coder_id",
        "historical_date", "code", "text_answer", "confidence", "data_type",
        "year")]

    return(outdf)

}


# Run script
if (no_test()) {
    
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    df <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]
    stopifnot(`df has zero rows` = nrow(df) > 0)
    qtable <- load_qtable()
    if (TASK_NAME != "vignettes") {
        qtable <- qtable[qtable$name == TASK_NAME, ]
    }
    country <- load_country()
    coder <- read_file(file.path(ROOT, "download", "coder.rds"))
    # Run
    collectedInputs <- named_list(TASK_NAME, df, qtable, country, coder)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>%
    write_file(., OUTFILE, dir_create = TRUE)
    info("Cleaning module finished.")
} else {
    # Tests
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_cleaning.R") %>%
		check_test_file_output()
}


