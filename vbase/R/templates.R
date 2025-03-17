#' @title Render a snippet of moustache templates for each row of a data.frame
#' @description This function takes a data.frame and a snippet of moustache templates
#' and renders the snippet for each row of the data.frame. The column names are matched 
#' for the moustache identifiers. See https://mustache.github.io/ for details
#' @param df A data.frame
#' @param snippet A character vector of moustache templates
#' @return A character vector of rendered snippets
#' @export
render_snippet <- function(df, snippet) {
    if (nrow(df) == 0) {
        return(NULL)
    }

    lapply(1:nrow(df), function(i) {
        out <- whisker::whisker.render(
            template = snippet,
            data = Filter(Negate(is.na), df[i, ])) 
        return(out) }
    )
}