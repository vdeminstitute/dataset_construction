#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Merge v3 data onto v2 variables.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------

# Ensure structure k across v2 and v3
assert_question_structure <- function(qtable) {

    k <- length(unique(qtable$k)); qClass <- all(qtable$class == "C")

    if (qClass & length(k) != 1) {
        stop("Contemporary and historical vary in their k-value")
    }
}

merge_reference_tables <- function(df, qtable, country_unit, country) {

    stopifnot(!with(df, anyNA(question_id, country_id)))
    inRow <- nrow(df)

    qtable <-
        qtable[, c("question_id", "name", "class", "hist_merge_conflict", "hist_merge_no_conflict")]
    country_unit <- country_unit[, c("country_id", "year", "project")]
    country <- country[, c("country_id", "codingstart_hist")]

    df <- merge(x=df, y=qtable, by="question_id", all.x=TRUE, sort=FALSE)
    stopifnot(inRow == nrow(df))
    inRow <- nrow(df)

    df <- merge(x=df, y=country_unit, by=c("country_id","year"), all.x=TRUE, sort=FALSE)
    stopifnot(inRow == nrow(df))
    inRow <- nrow(df)

    df <- merge(x=df, y=country, by="country_id", all.x=TRUE, sort=FALSE)
    stopifnot(inRow == nrow(df))

    df <- organiseRows(df, country_id, historical_date, coder_id)

    return(df)
}

# compare v2 and v3
compare_data <- function(df) {

    # Adjust project for pre-historical data
    df$project[df$class != "C" & !is.na(df$codingstart_hist) & df$year < df$codingstart_hist] <- "pre-historical"
    stopifnot(!anyNA(df$project))

    # Are the variables normal v2 or v3 variables?
    stopifnot(grepl("^v[23]", df$name))

    n_overlap_hist <- nrow(df[with(df, class != "C" & (hist_merge_conflict | hist_merge_no_conflict) & project %in% "overlap"), ])
    
    n_overlap_cont <- nrow(df[with(df, class != "C" & grepl("^v2", name) & project %in% "overlap"),])

    if (length(n_overlap_hist) > 0 && length(n_overlap_cont) > 0 && (n_overlap_hist > n_overlap_cont)) {
        info(sprintf("Historical overlap has more observations: [%d vs %d]",
            n_overlap_hist, n_overlap_cont))
    }
    return(df)
}

extract_historical <- function(df, TASK_NAME, qtable) {

    df <- df[grepl("^v3", df$name), ]
    stopifnot(df$hist_merge_conflict | df$hist_merge_no_conflict)

    # For non-C variables
    if (df$class[1] != "C") {
        stopifnot(grepl("^v2", TASK_NAME))
        overlap_use_hist <- qtable$overlap_use_hist[qtable$name == TASK_NAME]
        if (overlap_use_hist) {
            df <- df[df$project %in% c("pre-historical", "historical-only", "overlap"),]
        } else {
            df <- df[df$project %in% c("pre-historical", "historical-only"),]
        }
    }
    # For C variables
    if (df$class[1] == "C") {
        df <- df[df$project %in% c("historical-only", "overlap"), ]
    }

    stopifnot(grepl("^v3", df$name))
    return(df)
}

# translate question_id from v3 to v2
transform_historical_question_id <- function(df, qtable) {

    df[["question_id"]] <- vutils::normalize_qids(df[["question_id"]], qtable)
    stopifnot(!2446 %in% df$question_id)
    stopifnot(!any(is.na(df$question_id)))

    return(df)
}

extract_contemporary <- function(df, TASK_NAME, qtable) {
    
    df <- df[grepl("^v2", df$name), ]
    overlap_use_hist <- qtable$overlap_use_hist[qtable$name == TASK_NAME]
    if (overlap_use_hist) {
        df <- df[!df$project %in% "overlap",]
    }

    return(df)
}

# We do not want duplicated ratings
remove_duplicates <- function(df) {

    # Create unique id
    df$id <- seq_along(df$id)

    # identifiers
    id_cols <- c("question_id", "country_id", "historical_date")
    
    # Remove duplicates from A* A A,C B:
    if (df$class[1] != "C") {
        info("Cleaning duplicates for A* A A,C B variables")
        df <- remove_nonc_duplicates(
            df = df, col_names = id_cols, "remove_duplicates",
            "Duplicated observations across non-C variables removed")
    }

    # Remove duplicates from C data
    if (df$class[1] == "C") {

        info("Cleaning duplicates for C variables")
        id_cols <- c(id_cols, "coder_id")
        df <- remove_c_duplicates(
            df = df, col_names = id_cols, "remove_duplicates",
            "Duplicated observations across C-variables removed")
    }

    stopifnot(no_duplicates(df, c("country_id", "historical_date", "coder_id")))
    return(df)
}

# clean eltype after merge
eltype_cleanup <- function(df, TASK_NAME) {
    
    stopifnot(nrow(df) > 0)
    
    # Return if not v2eltype
    if (TASK_NAME != "v2eltype") {
        return(df)
    }

    inUnique <- unique(df[["text_answer"]])    
    df[["text_answer"]] <- gsub("[2389]", "", df[["text_answer"]])
    while (any(grepl(",,", df[["text_answer"]]))) {
        df[["text_answer"]] <- gsub(",,", ",", df[["text_answer"]])
    }

    df[["text_answer"]] <- trimws(df[["text_answer"]], "both", ",")
    stopifnot(!anyNA(df$text_answer))

    bool <- df[["text_answer"]] == ""
    stopifnot(`bool cannot have missingness` = !anyNA(bool))
    if (any(bool)) {
        df <- clean_observations(df, bool, "eltype_recoding",
            "Dropped if empty after eltype cleanup")
    }
    
    info(sprintf("Unique eltype values before [%s]",
        sort(toString(inUnique))))
    info(sprintf("Unique eltype values after [%s]",
        sort(toString(unique(df[["text_answer"]])))))

    return(df)
}

# Hardcoded lgbicam recoding
lgbicam_recoding <- function(df, TASK_NAME) {

    
    stopifnot(nrow(df) > 0)
    # Return if not v2lgbicam
    if (TASK_NAME != "v2lgbicam") {
        return(df)
    }

    info("Recoding lgbicam")
    # Ensure merged lgbicam conforms to contemporary coding rules.
    # -- Set 9 to 0: Other legislatures (ex: Monarch's advisory council)
    # -- set >2 to 2: -> Multichamber legislatures
    df[df$question_id == 595 & df$code == 9 & !is.na(df$code), "code"] <- 0
    df[df$question_id == 595 & df$code > 2 & !is.na(df$code), "code"] <- 2
    
    return(df)
}

main <- function(TASK_NAME, df, qtable, country_unit, country) {

    assert_question_structure(qtable)
    dfNames <- colnames(df)

    # Prepare data
    df <- compare_data(merge_reference_tables(df, qtable, country_unit, country))
    # Extract project components
    v3 <- extract_historical(df, TASK_NAME, qtable) |>
        transform_historical_question_id(df=_, qtable)
    v2 <- extract_contemporary(df, TASK_NAME, qtable)
    # Assemble and clean components
    out <- rbind.data.frame(v2, v3) |> 
        remove_duplicates(df=_) |> 
        eltype_cleanup(df=_, TASK_NAME) |> 
        lgbicam_recoding(df=_, TASK_NAME)

    dfKeep <- dfNames[dfNames %in% names(out)]
    dfKeep <- dfKeep[!grepl("project", fixed = TRUE, x = dfKeep)]

    out <- vbase::organiseRows(out[, dfKeep, drop = FALSE], country_id, coder_id, historical_date)
    
    return(out)
}

# Run script
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    V3NAME <- gsub("v2", "v3", TASK_NAME)
    country_unit <- load_country_unit()
    country_unit[["historical_date"]] <- NULL
    country <- load_country()
    names(country) <- gsub("name", "country_name", names(country))
    qtable <- load_qtable()
    qtable <- qtable[qtable$name %in% c(TASK_NAME, V3NAME), ]
    objs <- find_dep_files(TASK_ID, db)
    # df is a rowbind between v2 and v3
    df <- rbind.data.frame(
        objs[[TASK_NAME]][[TASK_NAME]],
        objs[[V3NAME]][[V3NAME]])
    stopifnot(nrow(qtable) == 2)

    # Run
    collectedInputs <- named_list(country_unit, country, qtable, df, TASK_NAME)
    setNames(list(do.call(main, collectedInputs)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)    
    info("Done with cont_hist_merge...")
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_cont_hist_merge.R") %>%
		check_test_file_output()
}
