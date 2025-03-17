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