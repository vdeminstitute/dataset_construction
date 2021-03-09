#' @export
replace_color_line <- function(s, word, front_color, back_color) {
    if (!grepl(word, s)) return(s)
    # Could also define background with
    # make_style(color, bg = TRUE)
    # combine_styles()
    # Check Rs built in colors() for possibilities
    # Can even use the rgb function for colors!
    front_style <- crayon::make_style(front_color)
    back_style <- crayon::make_style(back_color, bg = TRUE)
    f <- crayon::combine_styles(front_style, back_style)
    # f <- match.fun(color)
    gsub(word, f(word), s)
}

#' @export
color_cat <- function(s, words, front_colors, back_colors = "#000000", ...) {
    back_colors <- rep(back_colors, length(front_colors))
    for (n in 1:length(words)) {
        for (k in 1:length(s)) {
            s[k] <- replace_color_line(s[k], words[n], front_colors[n], back_colors[n])
        }
    }
    s %>%
        append_newline %>%
        combine_string %>%
        cat(., ...)
}

#' @export
color_cat_hex <- function(v, ...) {
    color_cat(v, words = v, front_colors = v, ...)
    v
}
