#!/usr/bin/env Rscript

# ==========================================================================
# Add -01-01 dates: works on C, A, A*, and B variables
# DESCRIPTION: This script adds historical dates corresponding to January 1
# for a given year.
#
# The purpose of this is to simplify the interpolation of ratings so that 
# the interpolation only needs to be done going forwards and not back-
# wards (within the year) and then forwards across years.
#
# Currently, we add these dates for all variables under all circumstances,
# except when there already exists a score for yyyy-01-01 for a the rele-
# vant grouping.
#
# We duplicate the value from code or text_answer when the only observation 
# for the grouping in question has the historical_date == yyyy-12-31 and the
# default date for the variable is yyyy-12-31. For the variables that have
# default dates corresponding to yyyy-01-01, and a value for yyyy-12-31,
# but for some reason does not have any value for yyyy-01-01, we do not copy
# the value from yyyy-12-31 but insert a row with NA for the value column.
# If the default date is yyyy-12-31, but within the year there are inserted
# values for other historical dates, we insert a new row for yyyy-01-01 but
# set the value to NA and then let the interpolation take care of the assignment
# of value to the observation in question.
#
# The interpolation is done by using a column that is created by the script 
# that identifies consecutive time periods, so that the interpolation only
# takes place for coherent time periods (in years), for a given subset.
#
# After interpolation, the very first observation of a consecutive period can
# contain a missing value for the value column.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# DESCRIPTION: We do this for all non-election-specific variables as the rules
# for them are different.
stop_if_els <- function(df, qtable, TASK_NAME) {

    stopifnot(`df is not a data.frame` = is.data.frame(df))
    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`qtable is not a data.frame` = is.data.frame(qtable))
    stopifnot(`qtable has zero rows` = nrow(qtable) > 0)
    stopifnot(`TASK_NAME is missing` = !(TASK_NAME == " " | TASK_NAME == "" | nchar(TASK_NAME) == 0))
    stopifnot(`Should not be applied to election-specific variables` = !qtable[["els"]])

    return(df)
}

# DESCRIPTION: helper to define grouping structure, given the class
group_by_varclass <- function(var_class, exclude_year = FALSE, exclude_consecutive = TRUE) {

    stopifnot(`question class is not correct`= var_class %in%  c("A","A*","B","D","A,C","C"))
    stopifnot(`exclude_year cannot be missing` = !is.na(exclude_year))
    stopifnot(`exclude_year needs to be length one` = length(exclude_year)==1)
    stopifnot(`exclude_year needs to be logical` = is.logical(exclude_year))
    stopifnot(`exclude_consecutive cannot be missing` = !is.na(exclude_consecutive))
    stopifnot(`exclude_consecutive needs to be length one` = length(exclude_consecutive)==1)
    stopifnot(`exclude_consecutive needs to be logical` = is.logical(exclude_consecutive))
    
    grouping_structure <- switch(
        EXPR = var_class,
        "A" = bquote(expr = group_by(., country_id, year, consecutive_period)),
        "A*" = bquote(expr = group_by(., country_id, year, consecutive_period)),
        "B" = bquote(expr = group_by(., country_id, year, consecutive_period)),
        "D" = bquote(expr = group_by(., country_id, year, consecutive_period)),
        "A,C" = bquote(expr = group_by(., country_id, year, consecutive_period)),
        "C" = bquote(expr = group_by(., country_id, coder_id, year, consecutive_period))
    )

    if (exclude_consecutive) {
        if (var_class == "C") {
            grouping_structure[[6]] <- NULL
        }
        if (var_class %in% c("A","A*","B","D","A,C")) {
            grouping_structure[[5]] <- NULL
        }
    }

    if (exclude_year) {
        if (var_class == "C") {
            grouping_structure[[5]] <- NULL
        }
        if (var_class %in% c("A","A*","B","D","A,C")) {
            grouping_structure[[4]] <- NULL
        }
    }

    stopifnot(`grouping_structure cannot be evaluated` = is.call(grouping_structure))

    return(grouping_structure)
}


# DESCRIPTION: Function that find places to insert january first dates.
# The rule is to insert into slices that do not already have
# january first date.
find_insertions <- function(df, qtable) {
    
    stopifnot(`Input needs to be data.frame` = is.data.frame(df))
    stopifnot(`df cannot have zero rows` = nrow(df) > 0)
    var_class <- qtable[["class"]]
    stopifnot(`varclass needs to character and non-missing` =
        class(var_class) == class(character()) & length(var_class) == 1)
    stopifnot(`question class is not correct`=
        var_class %in%  c("A","A*","B","D","A,C","C"))
    stopifnot(`country_id, coder_id, or year is/are missing` = 
        c("country_id", "coder_id", "year") %in% names(df))

    df <- df %>% 
        mutate(
            jan_1 = substr(historical_date, 6, 10) == "01-01",
            dec_12 = substr(historical_date, 6, 10) == "12-31") %>% 
        eval(expr = group_by_varclass(var_class)) %>%
        mutate(
            # NOTE: jan_1_exists determines where we CANNOT insert new values
            jan_1_exists = any(jan_1),
            # NOTE: dec_12_only determines if we want to bring the code/text_ans
            # Should be evaluated with dd_0101, that is true for variables that 
            # have January 1 as their default date.
            dec_12_only = all(dec_12) & n() == 1) %>%
        ungroup() %>% 
        as.data.frame()

    return(df)
}

# DESCRIPTION: Remove all groups where there already exists January first
filter_jan_1 <- function(df) {

    stopifnot(`df is not a data.frame` = is.data.frame(df))
    stopifnot(`df has zero rows` = nrow(df) > 0)
    stopifnot(`column jan_1_exists does not exist`= "jan_1_exists" %in% names(df))
    
    df_sub <- filter(df, !any(jan_1_exists))

    if (nrow(df_sub) > 0) {
        info("Returning filtered data")
        return(df_sub)
    } else {
        info("There are no January firsts, returning input")
        return(df)
    }
}

# DESCRIPTION: Insert yyyy-01-01 into the grouping structure where the are no
# already existing.
# 
# We cannot carry the value from yyyy-12-31 if the default date is yyyy-01-01.
# 
# The grouping structure can have more than one row. Hence, be aware of the 
# possibility of cases where there are multiple rows.
create_january_01 <- function(df, qtable, val_col, default_date_0101) {
    
    stopifnot(`Cannot find critical columns` =
        c("jan_1", "jan_1_exists", "dec_12", "dec_12_only") %in% names(df))
    var_class <- qtable[["class"]]
    stopifnot(`var_class should be a character and non-missing` =
        class(var_class) == class(character()) & length(var_class) == 1)
    stopifnot(`question class is not correct` = var_class %in%  c("A","A*","B","D","A,C","C"))
    stopifnot(`default_date_0101 needs to of length 1` = length(default_date_0101) == 1)
    stopifnot(`default_date_0101 cannot be missing` = !is.na(default_date_0101))

    # NOTE: This section needs to handle that the code filter(!any(jan_1_exists))
    # can remove all rows. This happens in one exact situation, where
    # there is not a single yyyy-01-01 in any of the groups.
    sub_df <- df %>%
        # group by given structure
        eval(expr = group_by_varclass(var_class)) %>% 
        # _remove_ all groups where there already is a 01-01
        filter_jan_1(df = .) %>% 
        # Keep unique rows for maximal grouping structure
        # if there for some reason exists duplicates
        distinct(country_id, coder_id, year, .keep_all = TRUE) %>% 
        # NOTE: Get one row per grouping structure
        filter(historical_date == max(historical_date)) %>% 
        ungroup() %>% 
        as.data.frame()

    # NOTE: the slices that are left should all get January first added.
    # We carry the value from value_col based on two conditions:
    # 1. yyyy-12-31 is the only observation
    # AND
    # 2. the default date for the variable is NOT January 1.
    # In plain, we copy the value for code or text_answer for variables 
    # where default date is not January 1 and the only observation that we 
    # have is yyyy-12-31.
    jan01_df <-
        data.frame(
            id = as.numeric(NA),
            question_id = unique(sub_df[["question_id"]]),
            country_id = sub_df[["country_id"]],
            coder_id = sub_df[["coder_id"]],
            historical_date = sub_df[["historical_date"]],
            # Conditional insertions
            code = as.numeric(ifelse(
                test = (sub_df[["dec_12_only"]] & val_col == "code") &
                        !default_date_0101,
                yes = sub_df[["code"]],
                no = NA)),
            text_answer = as.character(ifelse(
                test = (sub_df[["dec_12_only"]] & val_col == "text_answer") &
                    !default_date_0101,
                yes = sub_df[["text_answer"]],
                no = NA_character_)),
            confidence = ifelse(
                test = sub_df[["dec_12_only"]] & !default_date_0101,
                yes = sub_df[["confidence"]],
                no = NA_integer_),
            year = sub_df[["year"]],
            data_type = sub_df[["data_type"]],
            jan_1 = sub_df[["jan_1"]],
            jan_1_exists = sub_df[["jan_1_exists"]],
            dec_12 = sub_df[["dec_12"]],
            dec_12_only = sub_df[["dec_12_only"]]) %>%
        # Create January first observations
        mutate(historical_date = as.Date(paste0(year, "-01-01"), format = "%Y-%m-%d"))
        # Save to added_rows in the global environment.
        added_rows <<- jan01_df[, c(
            "country_id",
            "coder_id",
            "historical_date",
            "code",
            "text_answer",
            "confidence")]

    if (default_date_0101) {
        stopifnot(
            `For variables with 01-01 as default date, we should not copy the
            value for newly inserted rows` = allNA(jan01_df[[val_col]]))
    }

    info(sprintf("Added %d rows with yyyy-01-01 dates", nrow(jan01_df)))

    return(jan01_df)
}

# DESCRIPTION: Combine the created 01-01 df with the original df.
combine_dfs <- function(new_dates, original_df = df) {

    stopifnot(`Found new dates that are not January first` = all(
        substr(x = new_dates[["historical_date"]], start = 6, stop = 10) ==
        "01-01"))
    
    # Put together the original df and the new_dates
    new_df <-
        dplyr::bind_rows(original_df, new_dates) %>% 
        dplyr::arrange(country_id, historical_date) %>% 
        dplyr::select(-jan_1, -jan_1_exists, -dec_12, -dec_12_only)
    
    stopifnot(`Number of rows is not what is expected` = 
        nrow(new_df) == (nrow(original_df) + nrow(new_dates)))
    stopifnot(`country_id is unsorted`=
        !is.unsorted(new_df[["country_id"]], strictly=FALSE))
    split(new_df[["historical_date"]], f = new_df[["country_id"]], drop=TRUE) %>% 
        lapply(X=., function(x) {
            !is.unsorted(x,strictly=FALSE) }) %>% 
        unlist() %>%  {
            stopifnot(`historical_date is unsorted within countries` = .) }

    # Carry through the unique form of data_type
    data_type_insert <- unique(
        new_df[["data_type"]][!is.na(new_df[["data_type"]])])
    stopifnot(length(data_type_insert) == 1)
    new_df[["data_type"]][is.na(new_df[["data_type"]])] <- data_type_insert

    stopifnot(is.data.frame(new_df))

    return(new_df)
}

# DESCRIPTION: Rating id needs to be unique and non-missing.
create_unique_id <- function(df) {

    # Insert new rating_id
    df[["id"]][is.na(df[["id"]])] <- seq(
        from = max(df[["id"]], na.rm = TRUE) + 1,
        length.out = sum(is.na(df[["id"]])))

    stopifnot(`We do need a unique identifier for each rating` =
        !any(duplicated(df[["id"]])))
    stopifnot(`id cannot be missing` = !anyNA(df[["id"]]))

    return(df)
}

# DESCRIPTION: Helper for add_consecutive_year_period
make_consecutive_id <- function(vec) {

    stopifnot(`vec cannot have missingness` = !anyNA(vec))
    stopifnot(`argument to vec needs to be numeric/integer` = is.numeric(vec) | is.integer(vec))
    stopifnot(`vec needs to be at least of length 1` = length(vec) > 0)
    stopifnot(`vec cannot be of class matrix` = !is.matrix(vec))
    
    uniq_vec <- unique(vec)

    if (length(uniq_vec) == 1) {
        simple_out <- rep(1,times=length(vec))
        return(simple_out)
    }

    stopifnot(`argument supplied to vec needs to be sorted` = !is.unsorted(uniq_vec, strictly=FALSE))
    seq_id_list <- split(x=uniq_vec, f = cumsum(c(1, diff(uniq_vec) != 1)))
    
    out_vec <-
        Map(function(x, xnm) {
            period_id <- as.numeric(xnm)
            period_id_seq <- rep(period_id, n.times=length(x))
            out <- data.frame(
                year=x,
                consecutive_id=period_id_seq)
            stopifnot(is.data.frame(out))
            stopifnot(unique(out[["consequtive_id"]])==1)
            return(out)

        }, x = seq_id_list, xnm = names(seq_id_list)) %>% 
        do.call(rbind.data.frame, args = .) %>%
        merge(x=.,y=data.frame(year=vec), by = "year", all.y=TRUE) %>% 
        getElement("consecutive_id")

    stopifnot(is.vector(out_vec))
    stopifnot(length(vec)==length(out_vec))

    return(out_vec)
}

# DESCRIPTION: Add consecutive_year_period

add_consecutive_year_id <- function(df, qtable) {

    var_class <- qtable[["class"]]
    stopifnot(`varclass needs to be character and non-missing` =
        class(var_class) == class(character()) & length(var_class) == 1)
    stopifnot(`question class is not correct`=
        var_class %in%  c("A","A*","B","D","A,C","C"))
    stopifnot(`df has zero rows` = nrow(df) > 0)

    df <-
        df %>%   
        eval(expr = group_by_varclass(var_class, exclude_year=TRUE)) %>% 
        mutate(consecutive_period = make_consecutive_id(vec=year)) %>% 
        ungroup() %>% 
        as.data.frame()

    return(df)
}

# Interpolate across consecutive periods
interpolate_across_consecutive_periods <- function(df, qtable, val_col) {

    var_class <- qtable[["class"]]
    stopifnot(`varclass needs to be character and non-missing` =
        class(var_class) == class(character()) & length(var_class) == 1)
    stopifnot(`question class is not correct`=
        var_class %in%  c("A","A*","B","D","A,C","C"))
    stopifnot(`df has zero rows` = nrow(df) > 0)

    # Confidence needs to be interpolated for C variables
    if (var_class == "C") {
        locf_cols <- c(val_col, "confidence")
    } else {
        locf_cols <- val_col
    }

    df <-
        df %>%
        eval(expr = group_by_varclass(
            var_class,
            exclude_year = TRUE,
            exclude_consecutive = FALSE)) %>%
        # Interpolate across consecutive period
        mutate(across(.cols = all_of(locf_cols), .fns = ~ locf(.x))) %>% 
        arrange(country_id, historical_date) %>% 
        ungroup() %>% 
        as.data.frame()

    # Inserting January first will, despite interpolating, create some NAs on
    # confidence/code/text_answer under at least two circumstances:
    # 1) in the beginning of a consecutive period that also is the first 
    # consecutive period and where december 31 is not the only observation or 
    # that there is no 12-31 observation.
    #
    # 2) if a coder has removed 12-31 as default and inserted her own custom
    # date. This happened with e.g. Lithuania in 1990.
    #
    # Reduce do insert missing values from casting to wide.

    df[["consecutive_period"]] <- NULL

    return(df)

}

# DESCRIPTION: Main function
main <- function(TASK_NAME, df, qtable, val_col, default_date_0101) {

    out_df <- 
        stop_if_els(df, qtable, TASK_NAME) |> 
        find_insertions(df=_, qtable) |>  
        create_january_01(df=_, qtable, val_col, default_date_0101) |> 
        combine_dfs(new_dates = _, original_df = df) |> 
        create_unique_id(df=_) |>  
        add_consecutive_year_id(df=_, qtable) |> 
        interpolate_across_consecutive_periods(df=_, qtable, val_col)

    stopifnot(`Output from main is not a data.frame` =
        is.data.frame(out_df))
    stopifnot(`Output from main should not be grouped` = 
        !is.grouped_df(out_df))
    
    return(out_df)
}

# Run script
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()
    added_rows <- list()

    # Imports
    df <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]
    codebook <- load_codebook()
    codebook <- codebook[codebook$tag == TASK_NAME, ]
    country <- load_country()
    coder <- read_file(file.path(ROOT, "download", "coder.rds"))
    qtable <- load_qtable()
    qtable <- qtable[qtable$name == TASK_NAME, ]
    val_col <- ifelse(qtable$code_col, "code", "text_answer")
    default_date_0101 <- grepl(pattern = "January 1", fixed = TRUE, x = codebook[["defaultdate"]])

    stopifnot(length(val_col) == 1 & !is.na(val_col) & is.character(val_col))
    stopifnot(length(default_date_0101) == 1 & !is.na(default_date_0101) & is.logical(default_date_0101))

    # Run
    collectedInputs <- named_list(TASK_NAME, df, qtable, val_col, default_date_0101)
    setNames(list(do.call(main, collectedInputs), added_rows, DIRTYLIST),
        c(TASK_NAME, "added_rows", "dirt")) %>%
        write_file(., OUTFILE, dir_create = TRUE)
    info("Done with inserting January first dates")
} else {
    # Tests
    testthat::test_file("~/proj/vdemds/module_unit_tests/init/test_add_jan_dates.R") %>%
		check_test_file_output()
}
