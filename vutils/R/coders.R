#' Traverse coder-country network
#'
#' For a specific C variable and from a given start country,
#' \code{traverse} returns a DataFrame of countries accessible through
#' bridge/lateral coding.
#'
#' @param x \code{DataFrame} long-formatted coder-level data
#' @param root Country_id start position
#'
#' @details Ideally, for a particular C variable, every country (node)
#'     should be linked through bridge/lateral coding. Countries which
#'     do not appear in the output \code{DataFrame} are isolated
#'     nodes.
#'
#' @export
traverse <- function(x, root = 20) {
    aux <- data.frame(country_id = NULL, count = NULL)

    # For each country node, loop over each coder and for
    # every child country node if we haven't marked it yet with our
    # ad-hoc `aux` "stack", recursively descend.
    recurse <- function(parent = root) {
        coders <- x$coder_id[x$country_id == parent]

        for (c_coder in coders) {
            countries <- x$country_id[x$coder_id == c_coder & x$country_id != parent]

            if (length(countries) > 0) {
                for (c_country in countries) {
                    if (c_country != root & !c_country %in% aux$country_id) {
                        aux <<- rbind(aux, data.frame(country_id = c_country, count = 1))

                        recurse(c_country)
                    } else {
                        aux$count[aux$country_id == c_country] <<-
                            aux$count[aux$country_id == c_country] + 1
                    }
                }
            }

        }
    }

    recurse
    return(aux)
}


#' @title Delete a question from \code{coder_table}
#' @param question_id A V-Dem \code{question_id}
#' @param db A dataset construction database connection
#' @return Returns NULL, invisibly.
#' @details This function deletes a question from the \code{coder_table} given a valid question id.  It is used in the dataset construction module \code{gen_coder_tables.R}.
#' 
#' @examples
#' \dontrun{
#' db <- vbase::pg_connect("v13")
#' delete_coder_question(2500, db) 
#' }
#'
#' @export
delete_coder_question <- function(question_id, db) {
    DBI::dbGetQuery(
        conn = db,
        statement = sprintf("DELETE FROM coder_table WHERE question_id=%d;", question_id)
    )
}

#' @title Calculate the number of coders done with their assigned surveys
#' @param version The version of the dataset to calculate the completion rates for.
#' @param survey_ids A vector of survey IDs to include in the calculation.
#' @param cutoff A numeric value between 0 and 1 representing the completion cutoff.
#' #' @return A data.frame with the coder, survey, and country of coders who have completed their assigned surveys.
#' @details This function calculates which country-surveys coders have completed. This is calculated based on their assigned country-surveys (task table) and the number of questions answered (log table). It filters out coders who have not completed at least the specified cutoff percentage of their assigned surveys. The function returns a data.frame with coder_id, survey_id, and country_id, and survey_name.
#' 
#' # @examples
#' \dontrun{
#' coders_done <- calc_coder_done(version = 15, survey_ids = c(2057, 2058), cutoff = 0.95)
#' }
#'
#' @export
calc_coder_done <- function(version, survey_ids = NULL, cutoff = NULL) {
    vdem_data <- pg_connect("vdem_data")

    if (!is.numeric(version)) {
        warning("Version argument must be numeric.")
    }
    stopifnot(is.numeric(version))
    
    response_year <- version - 10
    timestamp_min <- sprintf("202%d-01-01", response_year)
    timestamp_max <- sprintf("202%d-12-31", response_year)

    if (is.null(survey_ids)) {
        survey_ids <- c(2057, 2058, 2059, 2060, 2061, 2062, 2063, 2064, 2065, 2066, 2097, 2099, 2101, 2104)
    }

    if (is.null(cutoff)) {
        cutoff <- 0.9
    } else if (cutoff > 1 | cutoff < 0) {
        stop("Cutoff must be between 0 and 1.")
    }


    # get survey info
    surveys <- DBI::dbGetQuery(vdem_data, 
        sprintf("SELECT qq.survey_id, qq.question_id, q.name, s.name as survey_name FROM question_queue as qq
        JOIN question as q ON qq.question_id = q.question_id
        JOIN survey as s ON qq.survey_id = s.survey_id
        WHERE qq.survey_id IN (%s);", paste(survey_ids, collapse = ","))) %>%
        filter(!grepl("intro|commnt", name)) %>%
        filter(!name %in% c("v2elcomcom", "v2elcomsn", "v2excommhs", "v2excommhg", "v2excomex", "v2cacom"))

    # calc number of questions per survey
    nqs <- surveys %>%
        group_by(survey_id) %>%
        summarise(n_questions = n(), .groups = "drop")

    # get assigned country-surveys (task table)
    task_table <- DBI::dbGetQuery(vdem_data,
        sprintf("SELECT task_id, survey_id, country_id, coder_id FROM task 
        WHERE fmsession_id = %s
        AND coder_id NOT IN (SELECT coder_id FROM coder WHERE test_coder IS true OR team_coder IS true);", version))

    stopifnot(!anyNA(task_table))

    # get coder logs (list for save_ratings)
    log_table <- DBI::dbGetQuery(vdem_data,
        sprintf("SELECT log_id, task_id, coder_id, question_id, event, timestamp FROM log 
        WHERE timestamp BETWEEN '%s' AND '%s' 
        AND event = 'save_ratings'
        AND coder_id NOT IN (SELECT coder_id FROM coder WHERE test_coder IS true OR team_coder IS true)
        AND question_id IN (%s);", timestamp_min, timestamp_max, paste(surveys$question_id, collapse = ",")))

    stopifnot(unique(log_table$event) == "save_ratings")
    stopifnot(any(log_table$timestamp >= timestamp_min & log_table$timestamp <= timestamp_max))
    stopifnot(!anyNA(log_table$task_id))
    stopifnot(!anyNA(log_table$coder_id))

    # calculate completion rates
    percent_complete <- log_table %>%
        # issue with log table having incorrect question_id for the associated task_id in two cases
        filter(!log_id %in% c(8736368, 9331170)) %>%
        distinct(task_id, coder_id, question_id) %>%
        group_by(task_id, coder_id) %>%
            summarise(n_answered = n(), .groups = "drop") %>%
        left_join(task_table, by = c("task_id", "coder_id")) %>%
        select(-task_id) %>%
        filter(survey_id %in% survey_ids) %>%
        left_join(nqs, by = "survey_id") %>%
        mutate(per_complete = n_answered / n_questions)

    stopifnot(!anyNA(percent_complete$per_complete))
    
    if (any(percent_complete$per_complete < 0 | percent_complete$per_complete > 1)) {
        info("Some completion rates are outside the range [0, 1]. Returning all coders and their scores, check the data.")
        return(percent_complete)
    } else {
        info("All completion rates are within the range [0, 1]. Proceeding to filter coders based on cutoff.")
        coders_done <- percent_complete %>%
            filter(per_complete >= cutoff) %>%
            distinct(coder_id, survey_id, country_id) %>%
            left_join(distinct(surveys, survey_id, survey_name), by = "survey_id") 

        info(sprintf("Found %d coder-country-survey combinations completed to at least %.0f%% of the %d assigned surveys.", nrow(distinct(coders_done, coder_id, country_id, survey_id)), cutoff * 100, nrow(distinct(task_table, coder_id, survey_id, country_id))))
        return(coders_done)
    }
}

#' @title Calculate the percent of question coders have completed in their assigned surveys
#' @param version The version of the dataset to calculate the completion rates for.
#' @param survey_ids A vector of survey IDs to include in the calculation.
#' #' @return A DataFrame with the completion rates for each coder, survey, and country.
#' @details This function calculates the completion rates for coders based on their assigned surveys and the number of questions answered. 
#' 
#' # @examples
#' \dontrun{
#' coders_done <- calc_coder_done(version = 15, survey_ids = c(2057, 2058))
#' }
#'
#' @export
calc_percent_done <- function(version, survey_ids = NULL) {
    vdem_data <- pg_connect("vdem_data")

    if (!is.numeric(version)) {
        warning("Version argument must be numeric.")
    }
    stopifnot(is.numeric(version))
    
    response_year <- version - 10
    timestamp_min <- sprintf("202%d-01-01", response_year)
    timestamp_max <- sprintf("202%d-12-31", response_year)

    if (is.null(survey_ids)) {
        survey_ids <- c(2057, 2058, 2059, 2060, 2061, 2062, 2063, 2064, 2065, 2066, 2097, 2099, 2101, 2104)
    }

    # get survey info
    surveys <- DBI::dbGetQuery(vdem_data, 
        sprintf("SELECT qq.survey_id, qq.question_id, q.name, s.name as survey_name FROM question_queue as qq
        JOIN question as q ON qq.question_id = q.question_id
        JOIN survey as s ON qq.survey_id = s.survey_id
        WHERE qq.survey_id IN (%s);", paste(survey_ids, collapse = ","))) %>%
        filter(!grepl("intro|commnt", name)) %>%
        filter(!name %in% c("v2elcomcom", "v2elcomsn", "v2excommhs", "v2excommhg", "v2excomex", "v2cacom"))

    # calc number of questions per survey
    nqs <- surveys %>%
        group_by(survey_id) %>%
        summarise(n_questions = n(), .groups = "drop")

    # get assigned country-surveys (task table)
    task_table <- DBI::dbGetQuery(vdem_data,
        sprintf("SELECT task_id, survey_id, country_id, coder_id FROM task 
        WHERE fmsession_id = %s
        AND coder_id NOT IN (SELECT coder_id FROM coder WHERE test_coder IS true OR team_coder IS true);", version))

    stopifnot(!anyNA(task_table))

    # get coder logs (list for save_ratings)
    log_table <- DBI::dbGetQuery(vdem_data,
        sprintf("SELECT log_id, task_id, coder_id, question_id, event, timestamp FROM log 
        WHERE timestamp BETWEEN '%s' AND '%s' 
        AND event = 'save_ratings'
        AND coder_id NOT IN (SELECT coder_id FROM coder WHERE test_coder IS true OR team_coder IS true)
        AND question_id IN (%s);", timestamp_min, timestamp_max, paste(surveys$question_id, collapse = ",")))

    stopifnot(unique(log_table$event) == "save_ratings")
    stopifnot(any(log_table$timestamp >= timestamp_min & log_table$timestamp <= timestamp_max))
    stopifnot(!anyNA(log_table$task_id))
    stopifnot(!anyNA(log_table$coder_id))

    # calculate completion rates
    percent_complete <- log_table %>%
        # issue with log table having incorrect question_id for the associated task_id in two cases
        filter(!log_id %in% c(8736368, 9331170)) %>%
        distinct(task_id, coder_id, question_id) %>%
        group_by(task_id, coder_id) %>%
            summarise(n_answered = n(), .groups = "drop") %>%
        left_join(task_table, by = c("task_id", "coder_id")) %>%
        select(-task_id) %>%
        filter(survey_id %in% survey_ids) %>%
        left_join(nqs, by = "survey_id") %>%
        mutate(per_complete = n_answered / n_questions)

    stopifnot(!anyNA(percent_complete$per_complete))
    
    if (any(percent_complete$per_complete < 0 | percent_complete$per_complete > 1)) {
        info("Some completion rates are outside the range [0, 1]. Returning all coders and their scores, check the data.")
        return(percent_complete)
    } else {
        info("All completion rates are within the range [0, 1].")

        df_out <- percent_complete %>%
            select(coder_id, survey_id, country_id, per_complete)

        return(df_out)
    }
}
