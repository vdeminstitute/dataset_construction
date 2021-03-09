#' Download V-Dem data from PostgreSQL in wide format for a variable
#'
#' \code{tbl_wide} returns the data frame for specified variable from certain
#' PostgreSQL table in a certain database.
#'
#' @param question_df a data.frame with the columns question_id and name.
#'
#' @param df_schema a vector with two elements: name of the database and schema
#' from which you need to grab the data
#'
#' @param var a character vector of length 1 specifying the name of the variable
#' to filter the data.
#'
#' @param tbl_name optional argument specifying the name of the table from which
#' to download the data. The default value is NULL and in this case the name of
#' the table in PostgreSQL is equal to the name of the database.
#'
#'@examples
#' \dontrun{
#' # Do not run
#' library(vutils)
#' library(dbplyr)
#' library(dplyr)
#'
#' question_df <- read_file("~/proj/mm-prep/ROOT/refs/question_table.rds") %>%
#'     select(question_id, name)
#' db_schema <- c("v9", "dataset")
#' var <- "v2expathhs"
#' dfWide <- tbl_wide(question_df = question_df, db_schema = db_schema, var = var)
#' }
#'
#' @export
tbl_wide <- function(question_df, db_schema, var, tbl_name = NULL) {
    if(is.null(tbl_name)) {
        tbl_name <- db_schema[1]
    } else {
        tbl_name <- tbl_name
    }

    conn <- pg_connect(dbname = db_schema[1])
    on.exit(DBI::dbDisconnect(conn), add = TRUE, after = FALSE)

    df <- tbl(conn, in_schema(db_schema[2], tbl_name)) %>%
        filter(question_id == local(question_df$question_id[question_df$name == var])) %>%
        select(-question_id, -id) %>%
        collect(n = Inf)

    df_wide <-data.table::dcast(df, country_id + historical_date ~ var_name, value.var = "code")

    return(df_wide)
}
