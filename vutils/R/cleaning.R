#' @export
clean_by_utable <- function(x, utable, party = FALSE) {UseMethod("clean_by_utable")}

#' @export
clean_by_utable.default <- function(x, utable, party = FALSE) {
    stop("No defined method for this object!")
}

#' @export
clean_by_utable.data.frame <- function(x, utable, party = FALSE) {
    stopifnot(c("year", "country_id") %in% names(x))
    stopifnot(!any(is.na(x$year)))
    stopifnot(!any(is.na(x$country_id)))
    dplyr::inner_join(x, utable[, "country_id", "year"],
        by = c("country_id", "year")) %>%
    # This may remove tibble grouping, be careful!
    as.data.frame(stringsAsFactors = FALSE)
}

#' @export
clean_by_utable.matrix <- function(x, utable, party = FALSE) {
    assert_str(x, party)
    df <- data.frame(
        country_text_id = get_text_id(x, party),
        historical_date = get_date(x, party) %>%
        to_year %>%
        paste0("-12-31"),
        stringsAsFactors = FALSE) %>%
        dplyr::left_join(
            dplyr::select(utable, country_text_id,
                historical_date, project),
            by = c("country_text_id", "historical_date"))
    bool <- !is.na(df$project)
    x[bool, ]
}


#' @export
remove_observations <- function(df, remove_bool) {
    stopifnot(`There are missing values in the remove_bool vector!` = 
		!anyNA(remove_bool))
    dirt <- df[remove_bool, , drop = FALSE]

    list(clean = df[!remove_bool, , drop = FALSE], dirty = dirt)
}

#' @export
clean_observations <- function(df, bool, function_name, description) {
	
	stopifnot(`bool not same length as df` = length(bool) == nrow(df))
    ll <- remove_observations(df, bool)

    if (any(bool)) {
        info(paste0("[", function_name, 
					"] [", description, "] ", 
					nrow(ll$dirty)))
        # If not in unit testing mode send to DIRTYLIST
        if (!isTRUE(as.logical(Sys.getenv("UNIT_TESTS")))) {
            stopifnot(
                `DIRTYLIST does not exist in the global environment` = 
                exists("DIRTYLIST", envir = .GlobalEnv))
                
            DIRTYLIST[[function_name]][[description]] <<- ll$dirty
        }
    }
    return(ll$clean)
}


#' @export
remove_nonc_duplicates <- function(df, col_names, function_name, description) {
    stopifnot(all(col_names %in% names(df)) | "id" %in% names(df))
    # Check for C-variable
    if (!all(df$class %in% c("A*", "A", "A,C", "B", "D")))
        return(df)
    # Generate bool for removing non-C
    keep_nonc <- df %>%
        dplyr::filter(class %in% c("A*", "A", "A,C", "B", "D")) %>%
        dplyr::arrange(!!!syms(col_names), dplyr::desc(id)) %>%
        dplyr::distinct(!!!syms(col_names), .keep_all = T) %$% id
    remove_nonc <-
        df %>%
        dplyr::filter(class %in% c("A*", "A", "A,C", "B", "D")) %>%
        dplyr::filter(!id %in% keep_nonc) %>%
        pull(id)

    nonc_bool <- df$id %in% remove_nonc
    stopifnot(`Bool cannot have missingness` = !anyNA(nonc_bool))

    if (any(nonc_bool)) {
        df %<>% clean_observations(.,
            bool = nonc_bool, function_name, description)
    }
    
    stopifnot(`df needs to be a data.frame` = is.data.frame(df))

    return(df)
}

#' @export
remove_c_duplicates <- function(df, col_names, function_name, description) {
    stopifnot(all(col_names %in% names(df)) | "id" %in% names(df))
    # Check for non-C-variable
    if (!all(df$class %in% c("C")))
        return(df)
    stopifnot(length(unique(df$id)) == nrow(df))

    keep_c <-
        df %>%
        dplyr::arrange(!!!syms(col_names), dplyr::desc(id)) %>%
        dplyr::distinct(!!!syms(col_names),
            .keep_all = T) %>% 
        pull(id)
    remove_c <-
        df %>%
        dplyr::filter(!id %in% keep_c) %>%
        pull(id)

    c_bool <- df$id %in% remove_c
    stopifnot(`Bool cannot have missingness` = !anyNA(c_bool))

    if (any(c_bool)) {
        df %<>% clean_observations(.,
            bool = c_bool, function_name, description)
    }

    stopifnot(`df needs to be a data.frame` = is.data.frame(df))
    stopifnot(no_duplicates(df, col_names))
    
    return(df)
}

#' @export
remove_ms_duplicates <- function(df) {
    keep_ms <-
        df %>%
        dplyr::filter(question_type == "S") %>%
        dplyr::arrange(dplyr::desc(id)) %>%
        dplyr::distinct(question_id, country_id, coder_id, historical_date, code,
            .keep_all = T) %$% id
    remove_ms <-
        df %>%
        dplyr::filter(question_type == "S") %>%
        dplyr::filter(!id %in% keep_ms) %$% id
    remove_observations(df, df$id %in% remove_ms)
}

#' @export
print_by_name <- function(df) {
        df %>%
        dplyr::group_by(name, class) %>%
        dplyr::summarize(n = n()) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        as.data.frame(stringsAsFactors = F) %>%
        print
}

#' @export
print_by_country <- function(df) {
        df %>%
        dplyr::group_by(country_name) %>%
        dplyr::summarize(n = dplyr::n()) %>%
        dplyr::arrange(dplyr::desc(n)) %>%
        as.data.frame(stringsAsFactors = F) %>%
        print
}

#' @export
print_by_country_year <- function(df) {
        df %>%
        dplyr::group_by(country_name, year) %>%
        dplyr::summarize(n = dplyr::n()) %>%
        dplyr::arrange(country_name, year) %>%
        as.data.frame(stringsAsFactors = F) %T>%
        print
}

#' @export
fill_empty_columns_a <- function(df) {
	# Fill in missing values from merge
	df$coder_id[is.na(df$coder_id)] <- na.omit(df$coder_id)[1]
	df$question_id[is.na(df$question_id)] <- na.omit(df$question_id)[1]
	df$data_type[is.na(df$data_type)] <- na.omit(df$data_type)[1]
	df$confidence[is.na(df$confidence)] <- 100L
	df$year <- to_year(df$historical_date)
	df$id <- seq_along(df$id)
	return(df)
}

#' @export
add_cleaning_var_expand_rows <- function(df_to_clean, df_cleaning_var) {

	country_dates <- 
		dplyr::bind_rows(
			dplyr::select(df_to_clean, country_id, historical_date),
			dplyr::select(df_cleaning_var, country_id, historical_date)) %>%
		dplyr::distinct(.)

	df_to_clean %<>%
		dplyr::full_join(country_dates, by = c("country_id", "historical_date"))

	df <- dplyr::left_join(df_to_clean, df_cleaning_var, 
					by = c("country_id", "historical_date"))
	
	df %<>% fill_empty_columns_a

	return(df)
}

#' @export
interpolate_cleaning_var <- function(df, cols, utable) {
	interpolate_components(df, cols, utable, keep_nan = TRUE,
		coder_level = FALSE)
}

#' @export
add_cleaning_var <- function(df_to_clean, df_cleaning_var, join_cols = c("country_id", "historical_date")) {

	df <- dplyr::full_join(df_to_clean, df_cleaning_var, 
					by = join_cols)

	return(df)
}

#' @export
cleaning_set_to_nan <- function(df, bool, function_name, description, qtable) {
	
	if (qtable$code_col) {
		df$code[bool] <- NaN
	} else {
		df$text_answer[bool] <- "NaN"
	}

	info(paste0("[", function_name, 
				"] [", description, "] ", 
				length(bool)))

	# Yes we return the original df!
	# clean_observations is only used for the DIRTYLIST and printing the message!
    return(df)
}

#' @export
drop_extra_rows <- function(df) {
    stopifnot(`id does not exist in df` = 
        !is.null(df[["id"]]))
	df[!is.na(df$id), , drop = FALSE]
}