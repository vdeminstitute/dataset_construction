# These functions are all to be phased out from vbase
# They should all inform the user about that.

#'
inform_phase_out_vbase <- function(f) {
    info(sprintf("%s will cease to exist", f))
}


# COLORS:
#' @export
replace_color_line <- function(s, word, front_color, back_color) {
    inform_phase_out_vbase("replace_color_line")

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
    inform_phase_out_vbase("color_cat")

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
    inform_phase_out_vbase("color_cat_hex")

    color_cat(v, words = v, front_colors = v, ...)
    v
}

# datetime
#'  @export
as.date <- function(v) {
    inform_phase_out_vbase("as.date")
    out <- as.Date(v, format = "%Y-%m-%d")
    return(out)
}

# datatypes
#' @export
char_to_num <- function(df) {
    inform_phase_out_vbase("char_to_num")

	df[] <- lapply(df, function(x) {
		if(is.character(x)) {
			return(as.numeric(as.factor(x)))
		}
		return(as.numeric(x))
	})

	return(df)
}

#' @export
posix_to_date <- function(df) {
    inform_phase_out_vbase("posix_to_date")

	df[] <- lapply(df, function(x) {
		if(class(x)[1] == "POSIXct") {
			x <- as.Date(x, format = "%Y-%m-%d")
		}
		return(x)
	})

	return(df)
}


# data.frame

#' @export
missing_value_freq <- function(df, cols, all_cols = FALSE) {

    inform_phase_out_vbase("missing_value_freq")

    stopifnot(is.data.frame(df))
    if(!all_cols) {
        stopifnot(is.character(cols))
        stopifnot(length(cols) <= ncol(df))
        stopifnot(any(cols %in% names(df)))
      
     cnt <- lapply(cols, function(v) {
        sum(is.na(df[[v]])) 
  } )
    info("Counting NAs on specified cols")
    return(unlist(setNames(object = cnt, nm = cols)))

    } else {
       all_cols <- names(df)
       
       cnt <- lapply(all_cols, function(v) {
        sum(is.na(df[[v]]))
    })
    info("Counting NAs on all columns")
    return(unlist(setNames(object = cnt, nm = all_cols)))
    }
}

#' @export
df_compare_keep_pairs_only <- function(df, group_cols) {

    inform_phase_out_vbase("df_compare_keep_pairs_only")

    split(df, lapply(group_cols, function(v) df[[v]])) %>%
        lapply(., function(subdf) {
            if (nrow(subdf) != 2)
                return(NULL)
            return(subdf)
        }) %>% 
        dplyr::bind_rows(.) %>%
        sort_df(., group_cols)
}

#' @export
df_compare_pairs <- function(df, group_cols, result = c("data.frame", "colnames")) {

    inform_phase_out_vbase("df_compare_pairs")

    stopifnot("dfdiff" %in% names(df))
    stopifnot(result %in% c("data.frame", "colnames"), length(result) == 1)

    df <- sort_df(df, c(group_cols, "dfdiff"))
    df$dfdiff <- NULL
    v <- names(df)
    group_cols_bool <- names(df) %in% group_cols_bool


    split(df, lapply(group_cols, function(v) df[[v]])) %>%
        lapply(., function(pairdf) {
            print(pairdf[[group_cols[1]]][1] %^% pairdf[[group_cols[2]]][1])
            bool <- ((pairdf[1, ] != pairdf[2, ]) | (is.na(pairdf[1, ]) != is.na(pairdf[2, ])))
            if (result == "data.frame") {
                return(pairdf[, group_cols_bool | bool])
            }
            if (result == "colnames") {
                return(paste(v[which(bool)], collapse = ","))
            }            
        })
}

#' @export
recycle_df <- function(df, n) {
    inform_phase_out_vbase("recycle_df")
    lapply(1:n, function(i) {
        force(df)
    }) %>%
    dplyr::bind_rows(.)
}

#' @export
clean_names <- function(v) {
    inform_phase_out_vbase("clean_names")

	v_new <- 
		v %>%
		# Replace ,.- with _
		gsub("[,.-]", "_", .) %>% 
		# Replace spaces with _
		gsub(" ", "_", .) %>% 
		# To lower capital letters
		tolower %>%
		# Remove special characters, but keep underscores (^ negates)
		gsub("[^a-z_]", "", .) 

	# Check if after cleaning there are now duplicate column names
	stopifnot(`There are duplicate names after cleaning!:` = length(unique(v_new)) == length(v_new))
	
	return(v_new)
}

#' @export
summary_df <- function(df) {

    inform_phase_out_vbase("summary_df")

	pb <- utils::txtProgressBar(initial = 0, style = 3)
	ncols <- ncol(df)
	sum_df <- Map(function(v, nn, iter, ncols) {
		utils::setTxtProgressBar(pb, iter/ncols)		
		dff <- data.frame(
			name = nn,
			data_type = class(v)[1],
			na = sum(is.na(v)))
		
		if (class(v)[1] %in% c("integer", "numeric")) {
			dff$min <- min(v, na.rm = TRUE) %>% as.character
			dff$max <- max(v, na.rm = TRUE) %>% as.character
			dff$mean <- mean(v, na.rm = TRUE) %>% as.character
			dff$median <- median(v, na.rm = TRUE) %>% as.character
			dff$range <- (as.numeric(dff$max) - as.numeric(dff$min)) %>% as.character
		}

		if (class(v)[1] %in% "numeric") {
			dff$nan <- sum(is.nan(v)) %>% as.character
			dff$inf <- sum(is.infinite(v)) %>% as.character
		}

		# factor?

		if (class(v)[1] %in% "Date") {
			dff$min <- min(v, na.rm = TRUE) %>% as.character
			dff$max <- max(v, na.rm = TRUE) %>% as.character
		}

		if (class(v)[1] %in% "character") {
			dff$min <- min(v, na.rm = TRUE) %>% as.character
			dff$max <- max(v, na.rm = TRUE) %>% as.character
			dff$empty_string <- sum(v == "", na.rm = TRUE) %>% as.character
			dff$min_nchar <- min(nchar(v), na.rm = TRUE) %>% as.character
			dff$max_nchar <- max(nchar(v), na.rm = TRUE) %>% as.character
		}

		dff %<>% select(any_of(c("name", "data_type", "na", "min", "max", "mean", 
			"median", "range", "nan", "inf", "empty_string", "min_nchar", 
			"max_nchar")))

		return(dff)
	}, v = df, nn = names(df), iter = 1:ncols, ncols) %>%
		dplyr::bind_rows(.)
	
	close(pb)
	
	return(sum_df)
}

#' @export
analyze_df <- function(df) {
    inform_phase_out_vbase("analyze_df")
	out <- list()
	out$summary <- summary_df(df)

	out$corr <- 
		char_to_num(df) %>% 
		as.matrix %>%
		cor(., use = "pairwise.complete.obs") %>% 
		suppressWarnings %>%
		round(., 4)
    
	return(out)
}

#' @export
is_env_set <- function(ENVVAR) {
    inform_phase_out_vbase("is_env_set")
	return(Sys.getenv(ENVVAR) != "")
}

#' @export
get_env <- function(ENVVAR) {
    inform_phase_out_vbase("get_env")
	out <- Sys.getenv(ENVVAR)
	stopifnot(`Specified environment variable is not set` = out != "")
	return(out)
}

#' @export
set_env <- function(...) {
    inform_phase_out_vbase("set_env")
	if (!interactive()) {
		stop("Function set_env is only available in interactive mode.")
	}

    Sys.setenv(...)
}

# file
#' @export
dir.writeable <- function(s) {
    inform_phase_out_vbase("dir.writeable")

    dir.exists(s) & file.access(s, mode = 2) == 0
}

# list
#' @export
list_replicate <- function(obj, n) {
    inform_phase_out_vbase("list_replicate")

    ll <- vector("list", n)
    for (i in 1:n) {    
        ll[[i]] <- obj 
    }
    return(ll)
}

#' @export
safe <- function(obj) {
    inform_phase_out_vbase("safe")

    if(is.null(obj)) {
        stop("Object returns NULL!")
    }
    return(obj)
}

#' @export
list_remove_null <- function(ll) {
    inform_phase_out_vbase("list_remove_null")
    ll[!(lapply(ll, is.null) %>% unlist)]
}

#' @export
list_remove_try_error <- function(ll) {
    inform_phase_out_vbase("list_remove_try_error")
    ll[!(lapply(ll, is.try_error) %>% unlist)]
}

# logs
#' @export
quiet <- function(x) {
    inform_phase_out_vbase("quiet")
	sink(tempfile()) 
	on.exit(sink()) 
	invisible(force(x))  
}

# na
#' @export
set_NA <- function(obj, bool) {
    inform_phase_out_vbase("set_NA")
    if (is.matrix(bool))
        stop("bool is already a matrix!")
    is.na(obj) <- bool_to_matrix(bool, ncol(obj))
    return(obj)
}

#' @export
bool_to_matrix <- function(bool, n) {
    inform_phase_out_vbase("bool_to_matrix")
    matrix(rep(bool, n), ncol = n)
}

# operators
#' @export
`%>>%` <- function(x, y) {

    inform_phase_out_vbase("%>>%")

	stopifnot(
		`Object is not a list, do you want to use %>% ?` = 
			class(x)[1] == "list",
		`Object does not contain elements called clean and dirt!` = 
			c("clean", "dirt") %in% names(x),
		`dirt needs to be a named list!` = 
			class(x[["dirt"]])[1] == "list",
		`dirt needs to be a list of length one!`= 
			length(x[["dirt"]]) == 1,
		`dirt needs to be a named list with the function name as the name!` = 
			length(names(x[["dirt"]])) == 1)
	
	if (!isTRUE(as.logical(Sys.getenv("UNIT_TESTS")))) {
		stopifnot(`dirtylist does not exist in the global environment` = 
			exists("dirtylist", envir = .GlobalEnv))
		dirtylist[names(x$dirt)] <<- list(x$dirt[[1]])
	}

	return(eval(substitute(x[["clean"]] %>% y)))
}

#' @export
collect_dirt <- function(x) {

    inform_phase_out_vbase("collect_dirt")

	stopifnot(
		`Object is not a list, do you want to use %>% ?` = 
			class(x)[1] == "list",
		`Object does not contain elements called clean and dirt!` = 
			c("clean", "dirt") %in% names(x),
		`dirt needs to be a named list!` = 
			class(x[["dirt"]])[1] == "list",
		`dirt needs to be a list of length one!`= 
			length(x[["dirt"]]) == 1,
		`dirt needs to be a named list with the function name as the name!` = 
			length(names(x[["dirt"]])) == 1)
	
	if (!isTRUE(as.logical(Sys.getenv("UNIT_TESTS")))) {
		stopifnot(`dirtylist does not exist in the global environment` = 
			exists("dirtylist", envir = .GlobalEnv))
		dirtylist[names(x$dirt)] <<- list(x$dirt[[1]])
	}

	return(x[["clean"]])
}

#' @export
`%contains%` <- function(lhs, rhs) {
    inform_phase_out_vbase("%contains%")

    rhs %in% lhs
}

# packages
#' @export
install_packages_if_needed <- function(list.of.packages) {
    inform_phase_out_vbase("install_packages_if_needed")

	new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
	if(length(new.packages)) install.packages(new.packages)
}

# parallel
#' @export
all_forks_done <- function(pattern = "fork", forklist) {

    inform_phase_out_vbase("all_forks_done")

    if (!missing(forklist)) {
        ll <- parallel::mccollect(mget(forklist, envir = .GlobalEnv))
    } else {
        ll <- parallel::mccollect(mget(ls(pattern = "fork\\d+",
                                         envir = .GlobalEnv),
                                      envir = .GlobalEnv))
    }

    all(unlist(lapply(ll, is.null)))
}

#' @export
mc_error <- function(ll) {

    inform_phase_out_vbase("mc_error")

	which(lapply(ll, is.try_error) %>% unlist)
}

# strings
#' @export
append_empty_rows <- function(s, n = 1) {
    inform_phase_out_vbase("append_empty_rows")
    c(s, rep("\n", n))
}

#' @export
append_newline <- function(s) {
    inform_phase_out_vbase("append_newline")
    paste0(gsub("\n", "", s, fixed = T), "\n")
}

#' @export
combine_string <- function(s) {
    inform_phase_out_vbase("combine_string")
    paste(s, collapse = "")
}

#' @export
mlstr <- function(v) {
    inform_phase_out_vbase("mlstr")
    
    gsub("[\r\n\t]", " ", v) %>% 
    gsub("[[:space:]]+", " ", .) %>%
    trimws()

}

#' @export
combinations <- function(...) {
    inform_phase_out_vbase("combinations")
    
    apply(expand.grid(..., KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE), 1, 
        function(v) paste(v, collapse = "")) %>% sort
}

# system

#' @export
system2_ <- function(args = character(), stderr = TRUE, stdout = TRUE,
	env = character(), tty = TRUE, pipe_status = TRUE, 
	defensive = TRUE, ...) {

    inform_phase_out_vbase("system2_")

	if (tty) {
		args <- c(args, " | tee /dev/tty")
	}

	if (pipe_status) {
		args <- c(args, '; echo "${pipestatus[@]}"') # 
	}

	out <- system2(
		command = "zsh", 
		args = c('-c', shQuote(paste(args, collapse = " "))), 
		env = env, 
		stderr = stderr, 
		stdout = stdout, ...)

	# Set status attribute if it is not already there.
	if (is.null(attr(out, "status")))
		attr(out, "status") <- 0L

	# Extract pipestatus
	if (pipe_status) {
		pipe_status_value <- out[length(out)]
		out_attributes <- attributes(out)
		out <- out[-length(out)]
		attributes(out) <- out_attributes
		attr(out, "pipe_status") <- 
			as.integer(unlist(strsplit(pipe_status_value, " ")))
	}

	if (defensive) {
		if (attr(out, "status") != 0L) {
			print(paste("status:", attr(out, "status")))
			stop("system2_ failed due to exit status!")
		}

		if (pipe_status) {
			if (any(attr(out, "pipe_status") != 0L)) {
				print(paste("pipe_status:", 
							paste(attr(out, "pipe_status"), collapse = " ")
							))
				stop("system2_ failed due to pipe_status!")
			}
		}
	}

	return(out)
}

# vector
#' @export
remove_items <- function(v, items) {
    inform_phase_out_vbase("remove_items")
    not_contained <- items[!items %in% v]
    if (length(not_contained) > 0)
        print("These items do not exist in the vector: " %^% not_contained)
    v[!v %in% items]
}

#' @export
n_digits <- function(vec) {
    inform_phase_out_vbase("n_digits")
    stopifnot(is.vector(vec))
    if(is.numeric(vec)){ 
        cvec <- as.character(vec)
        nn <- nchar(trimws(gsub(
            pattern = "\\.|\\-|\\+", replacement = "", x = cvec),
            which = "both"),
            type = "chars")
        return(nn)
    } else {
        return(NULL)
        info("Only for numeric vectors but applied to non-numeric; use nchar() instead.")
    }
}

# wrappers
#' @export
table_ <- function(x, ...) {
    inform_phase_out_vbase("table_")
    table(x, useNA = "always", ...)
} 

#' @export
grep_ <- function(x, ...) {
    inform_phase_out_vbase("grep_")
    grep(x, value = TRUE, ...)
}