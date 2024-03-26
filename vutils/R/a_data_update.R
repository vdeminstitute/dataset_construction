#' Wrapper around postgres download
#'
#' This is much faster because it filters first by the question table!
#' @export
pg_download <- function(question, db) {

    stopifnot(`question is not a data.frame. Have you used collect?` =
        is.data.frame(question))
        
    stopifnot(`Wrong database connection. Pleases specify vdem_data in pg_connect` =
        DBI::dbExistsTable(db, "rating"))

    qids <- question$question_id

    question_tbl <- 
        dplyr::tbl(db, "question") %>% 
        dplyr::select(question_id, name, question_type) %>%
		dplyr::collect(n = Inf)
    
    question_tbl_download <- 
        dplyr::tbl(db, "question") %>%    
        dplyr::select(question_id, name, class) %>%
        dplyr::filter(question_id %in% qids) %>%
        dplyr::mutate(
			rating_tbl = is.na(class) | (!class %in% c("A*", "A", "A,C", "B", "B,C", "A,B", "D")),
			a_data_tbl = class %in% c("A*", "A", "A,C", "B", "B,C", "A,B", "D"))

    country <-
        dplyr::tbl(db, "country") %>%
        dplyr::select(country_id, country_text_id = text_id, country_name = name) %>%
		dplyr::collect(n = Inf)

    rating <-
        dplyr::tbl(db, "rating") %>%
        dplyr::inner_join(question_tbl_download, by = "question_id") %>%
        dplyr::filter(rating_tbl) %>%
        dplyr::select(-name, -class) %>% 
		dplyr::collect(n = Inf)

	a_data <- 
		dplyr::tbl(db, "a_data") %>%
		dplyr::inner_join(question_tbl_download, by = "question_id") %>%
        dplyr::filter(a_data_tbl) %>%
        dplyr::select(-name, -class) %>% 
		dplyr::collect(n = Inf)

    # Prepare output
    dfList <- list()

    if (nrow(rating) > 0) {
        dfList$rating <- rating
    }

    if (nrow(a_data) > 0 ) {
        dfList$a_data <- a_data
    }

    if (length(dfList) == 0) {
        stop(sprintf("No data found for question_id: %s", toString(qids)),
            call. = FALSE)
    }

    out <- 
        dplyr::bind_rows(dfList) %>%
        dplyr::left_join(country, by = "country_id") %>%
        dplyr::left_join(question_tbl, by = "question_id") %>%
        dplyr::select(name, country_name, coder_id, historical_date, code, 
            text_answer, confidence, dplyr::everything()) %>%
        dplyr::mutate(
            code_col = question_type %in% c("M", "N", "V", "Y", "R", "U"),
            text_col = question_type %in% c("T", "S", "D")) %>%
        dplyr::select(-question_type) %>% 
        untibble()
	return(out)
}


#' @export
load_a_data_batches <- function(BATCH_TAG, db) {
	a_data_coding <- 
		dplyr::tbl(db, "a_data_batches") %>% 
		dplyr::collect(n = Inf) %>%
		dplyr::filter(batch_tag == BATCH_TAG)

	
	with(a_data_coding, {
		stopifnot(`batch_tag cannot be missing!` = !is.na(batch_tag))
		stopifnot(`variables cannot be missing!` = !is.na(variables))
		stopifnot(`variable_order cannot be missing!` = !is.na(variable_order))
		stopifnot(`outdir cannot be missing!` = !is.na(outdir))
		stopifnot(`min_year cannot be missing!` = !is.na(min_year))
		stopifnot(`fill_new_year cannot be missing!` = !is.na(fill_new_year))
		stopifnot(`create_new_year cannot be missing!` = !is.na(create_new_year))
		stopifnot(`coder_id cannot be missing!` = !is.na(coder_id))
		stopifnot(`batch_tag does not end with two digits!` = 
			gsub("^(.*?)(\\d+)$", "\\2", batch_tag) %>% nchar(.) == 2)
		stopifnot(`reference_variables may not be empty string` = 
			is.na(reference_variables) |
			(!is.na(reference_variables) & 
				(reference_variables != "")))
	})
	return(a_data_coding)
}

#' @export
extract_vars_a <- function(v) {
	strsplit(v, split = ",") %>% 
		unlist %>% 
		trimws(., which = "both")
}

#' @export
pg_delete_a_data <- function(ids, db) {
	info("Deleting from a_data table...")
    df <- data.frame(default_id = NA_integer_,
                     rating_id = ids)
    pg_append_table(df, name = "temp_table_auto", db)

    DBI::dbGetQuery(db,
        "DELETE FROM a_data WHERE id IN " %^%
        "(SELECT rating_id FROM temp_table_auto);")

    pg_truncate_temp_table_auto(db)
}

#'@export
pg_truncate_temp_table_auto <- function(db) {
    DBI::dbGetQuery(db, "TRUNCATE TABLE temp_table_auto;")
}