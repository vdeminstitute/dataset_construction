#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Various cleaning procedures for all/most of the variables
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))
options(dplyr.summarise.inform = FALSE)

# Functions
# --------------------------------------------------------------------------
merge_reference_tables <- function(df, qtable, country) {

    stopifnot(`df has zero rows` = nrow(df) > 0)

    qtable <- qtable[, c("question_id", "name", "class", "hist_outside_coding",
        "cont_outside_coding", "question_type")]

    country <- country[, c("country_id", "name", "codingstart_hist")]
    colnames(country) <- sub("name", "country_name", x = names(country), fixed = TRUE)
    
    # Left merge with assertions that number of rows remain unchanged.
    inRow <- nrow(df)
    df <- merge(
            x = df,
            y = qtable,
            by = "question_id",
            all.x = TRUE)
    df <- merge(
            x = df,
            y = country,
            by = "country_id",
            all.x = TRUE)

    stopifnot(inRow == nrow(df))
    return(df)
}

# Drop observations with NA for historical_date
remove_historical_date_na <- function(df) {
    stopifnot(`df has zero rows` = nrow(df) > 0)
    
    info("Remove observations where historical_date is missing")
    remove_bool <- is.na(df[["historical_date"]])
    stopifnot(`bool cannot have missingness` = !anyNA(remove_bool))

    if (any(remove_bool)) {
        df <- clean_observations(df, remove_bool, "remove_historical_date_na",
            "Removing observations where historical_date is missing")
    }

    return(df)
}

merge_country_unit <- function(df, country_unit) {
    # Clean Dates by utable (some exceptions may apply)
    # Except left side for v3 A, A*, B variables.
    # These will be cleaned after prep_nonc.R
    
    stopifnot(`df has zero rows` = nrow(df) > 0)
    inRow <- nrow(df)
    df <- merge(
        x = df,
        y = country_unit[, !names(country_unit) %in% "historical_date", drop = FALSE],
        by = c("country_id", "year"),
        all.x =TRUE)
    stopifnot(inRow == nrow(df))
    return(df)
}

# Add pre-historical identifier
transform_historical <- function(df) {
    
    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`all v3* does not have codingstart_hist` =
        all(!(grepl("^v3", df$name) & is.na(df$codingstart_hist))))

    # Identify a pre-historical time period.
    # -- Needed for filling historical A A* B variables
    bool <- with(df, class != "C" & !is.na(codingstart_hist) & year < codingstart_hist)
    df$project[bool] <- "pre-historical"

    if (any(df$project[!is.na(df$project)] == "pre-historical")) {
        info("Variable has observations prior to the coding period. These are removed after being used to backfill")
        print_by_country(df[with(df, project == "pre-historical" & !is.na(project)), ])
    }
    return(df)
}

# Clean by unit table
# -- country_unit is merged in already
# -- three conditions: (1) v2, (2) v3, (3) v2/v3, (4) pre-historical
clean_by_country_unit <- function(df) {

    stopifnot(`country_name has missingness` = !anyNA(df[["country_name"]]))
    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`question_name has missingness` = !anyNA(df[["name"]]))
    
    # Contemporary
    if (grepl("^v2", df$name[1])) {

        bool <- grepl("^v2", df$name) &
            !df$cont_outside_coding &
            (is.na(df$project) |
            !df$project %in% c("overlap", "contemporary-only"))
        stopifnot(`bool cannot have missingness` = !anyNA(bool))
    
        if (any(bool)) {
            df <- clean_observations(df, bool, "clean_by_country_unit",
                    "Removing v2-obs outside unit")
        }
        rm(bool)
    }

    # Historical (not pre-)
    if (grepl("^v3", df$name[1])) {

        bool <- grepl("^v3", df$name) &
            !df$hist_outside_coding &
            (is.na(df$project) |
                !df$project %in%
                    c("overlap", "historical-only", "pre-historical"))
        stopifnot(`bool cannot have missingness` = !anyNA(bool))
        
        if (any(bool)) {
            df <- clean_observations(df, bool, "clean_by_country_unit",
                    "Removing v3-obs. outside units (not pre-historical)")
        }
        rm(bool)
    }

    # Extended (both v2/v3)
    bool <- grepl("^v\\d", df$name) &
        (df$hist_outside_coding | df$cont_outside_coding) &
        (is.na(df$project) |
        !df$project %in%
            c("overlap","historical-only","pre-historical","contemporary-only"))
    stopifnot(`bool cannot have missingness` = !anyNA(bool))
    if (any(bool)) {
        df <- clean_observations(df, bool, "clean_by_country_unit",
                "Removing v2/v3-obs. outside units (extended)")
    }
    rm(bool)
    
	# Pre-historical observations
    exceptions <- c("v3eltype", "v3expathhs", "v3exhoshog", "v3lgbicam", "v3lgello", "v3expathhg")
	bool <- grepl("^v3", df$name) &
        (is.na(df$project) | df$project %in% c("pre-historical")) &
		(!df$name %in% exceptions)
    df <- clean_observations(df, bool, "clean_by_country_unit",
        "Removing pre-historical observations")


    return(df)
}

# Remove duplicated values
clean_duplicates <- function(df) {

    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`coder_id is missing` = !anyNA(df[["coder_id"]]))
    stopifnot(`country_id is missing` = !anyNA(df[["country_id"]]))
    stopifnot(`historical_date is missing` = !anyNA(df[["historical_date"]]))
    stopifnot(`question_id is missing` = !anyNA(df[["question_id"]]))
    stopifnot(`project is missing` = !anyNA(df[["project"]]))
    stopifnot(`class is missing` = !anyNA(df[["class"]]))
    stopifnot(`wrong class` = df[["class"]] %in%
        c("A*", "A", "A,C", "B", "C", "D"))

    # Remove duplicates by classes
    id_cols <- c("question_id", "country_id", "historical_date")
    # -- Remove duplicates from A* A A,C B:
    if (df[["class"]][1] %in% c("A*", "A", "A,C", "B", "D")) {
        # Keep the last row in a seq. of duplicates   
        info("Cleaning duplicates for A*, A, A,C, B, D variables")
        df <- remove_nonc_duplicates(df,
            col_names = id_cols, "clean_duplicates",
            "Removing duplicated values for non-C classes")
    }
    # -- Remove duplicates from C data
    if (df[["class"]][1] == "C") {
        # Keep the last row in a seq. of duplicates 
        info("Cleaning duplicates for C variables")
        id_cols <- append(id_cols, "coder_id")
        df <- remove_c_duplicates(df,
            col_names = id_cols, "clean_duplicates",
            "Removing duplicated observations for class C")
    }

    stopifnot(`id is not unique`= length(unique(df[["id"]])) == nrow(df))
    stopifnot(`There are duplicates` = no_duplicates(df,
        cols = c("country_id", "historical_date", "coder_id")))
    
    return(df)
}

# Clean percentage varables
percent_cleaning <- function(df) {

    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`question_type is missing` = !is.null(df[["question_type"]]))
    stopifnot(`question_type has missingness` = !anyNA(df[["question_type"]]))

    if (df[["question_type"]][1] == "R") {
		stopifnot(`code has missingness` = !anyNA(df[["code"]]))
        info("Clean out of bounds percentage values")
        bool <-
            df[["question_type"]] == "R" &
            (df[["code"]] > 100 | df[["code"]] < 0)
        stopifnot(`bool cannot have missingness` = !anyNA(bool))

        if (any(bool)) {
            df <- clean_observations(df, bool, "percent_cleaning",
                "Removing invalid percentage values")
        }
    }

    return(df)
}

# Special cleaning for v2mecenefi:
# If (1) year after 1993 -> keep
# If (2) |Exists| / |Ratings| >= 0.5 \forall from (1) -> keep
mecenefi_cleaning <- function(df, TASK_NAME) {
    
    if (TASK_NAME != "v2mecenefi") {
        return(df)
    }

    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`year is missing` = !is.null(df[["year"]]))

    info("Cleaning v2mecenefi based on internet existence")

    # (1)
    bool <- df[["name"]] == "v2mecenefi" &
        df[["historical_date"]] < "1993-01-01"
    stopifnot(`bool cannot have missingness` = !anyNA(bool))
    if (any(bool)) {
        df <- clean_observations(df, bool, "mecenefi_cleaning",
            "Removing observations before 1993")
        stopifnot(`there should be no obs. before 1993` = df[["year"]] >= 1993)
    }
    rm(bool)

    # (2)
    splits <- split(df, list(df$coder_id, df$country_id, df$year), drop = TRUE)
    maxYear <- do.call(rbind.data.frame, lapply(splits, function(x) {
        maxCode <- max(x$code)
        return(data.frame(
            coder_id = x$coder_id,
            country_id = x$country_id,
            year = x$year,
            code = maxCode)) }))
    rm(splits)
    splits <- split(maxYear, list(maxYear$country_id, maxYear$year), drop = TRUE)
    years <- do.call(rbind.data.frame, lapply(splits, function(x) {
        stopifnot(is.data.frame(x))
        bool <- with(x, (sum(code==0) / length(code)) >= 0.5)
        stopifnot(`bool cannot have missingness` = !anyNA(bool))
        stopifnot(length(bool) == 1)
        if (bool) {
            return(x[, c("country_id", "year")])
        } else {
            return(NULL)
        } }))
    rm(splits)
    # Columns to check by
    by_cols <- c("country_id", "year")
    # Boolean for if you are in df and years
    bool <- interaction(df[, by_cols]) %in% interaction(years[, by_cols])
    stopifnot(`bool cannot have missingness` = !anyNA(bool))
    if (any(bool)) {
        df <- clean_observations(df, bool,
            "mecenefi_cleaning",
            "Removing observation based on majority vote of experts")
    }
    return(df)
}

clean_confidence <- function(df) {

    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`project is missing` = !anyNA(df[["project"]]))
    stopifnot(`confidence larger than 100 exists` =
        !any(df[["confidence"]] > 100 & !is.na(df[["confidence"]])))

    info("Clean where class is C and confidence is zero")
    bool <- with(df, class == "C" & confidence == 0 & !is.na(confidence))
    stopifnot(`bool cannot have missingness` = !anyNA(bool))

    if (any(bool)) {
        df <- clean_observations(df, bool, "clean_confidence",
            "Removing observations with zero confidence")
    }
    stopifnot(`code has missingness`= !any(!is.na(df[["code"]]) & df[["code"]] < 0))
    return(df)
}

main <- function(TASK_NAME, qtable, country, country_unit, df) {

    stopifnot(length(unique(df[["id"]])) == nrow(df))

    outdf <-
        merge_reference_tables(df, qtable, country) %>%
        remove_historical_date_na(.) %>%
        merge_country_unit(., country_unit) %>%
        transform_historical(.) %>%
        clean_by_country_unit(.) %>%
        clean_duplicates(.) %>%
        percent_cleaning(.) %>%
        mecenefi_cleaning(., TASK_NAME) %>%
        clean_confidence(.)
    
    outdf <- outdf[, c("id","question_id","country_id","coder_id",
        "historical_date","code","text_answer","confidence","year","data_type")]

    return(outdf)
}

# Run script
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    qtable <- load_qtable()
    country <- load_country()
    country_unit <- load_country_unit()
    df <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]

    # Run
    collectedInputs <- named_list(TASK_NAME, qtable, country, country_unit, df)
    setNames(list(do.call(main, collectedInputs), DIRTYLIST), 
        c(TASK_NAME, "dirt")) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("Done with clean_all")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_clean_all.R") %>%
		check_test_file_output()
}
