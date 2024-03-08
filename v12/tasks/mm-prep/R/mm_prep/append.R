#!/usr/bin/env Rscript


options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
set_env(MODULE_NAME = "append")

vignette_id <- function(vtable, VARNAME) {
    filter(vtable, parent_name == VARNAME) %>%
        pull(vignette_id) %>%
        return(.)
}

successor_country <- function(ctable) {
    extinct <- ctable %>%
        filter(!is.na(parent_country_id)) %>%
        mutate(successor_text_id = to_ctext_ids(parent_country_id, ctable))

    successor_text_id <- with(extinct, setNames(successor_text_id, country_text_id))
    return(successor_text_id)
}

append_vignettes <- function(input.data, vig_data, vignette_ids, VARNAME) {
    stopifnot(c("wdata", "conf_mat") %in% names(input.data))
    if (length(vignette_ids) > 0) {
        vignettes.df <- vig_data %>%
            filter(parent_name == VARNAME) %>%
            filter(coder_id %in% colnames(input.data$wdata))

        if (nrow(vignettes.df) > 0) {
            vignettes.df %<>%
                select(id, coder_id, code) %>%
                spread(coder_id, code)

            vdata <- select(vignettes.df, -id) %>% data.matrix
            rownames(vdata) <- vignettes.df$id

            # Remember our data is 1-indexed
            vdata <- vdata + 1

            input.data$wdata %<>% full_rbind(vdata)

            # We actually don't need vignette reported confidences.
            # The reported confidences get cut off later.
            vdata[!is.na(vdata)] <- 1
            input.data$conf_mat %<>% full_rbind(vdata)
        }
    }
    stopifnot(nrow(input.data$wdata) > 0, nrow(input.data$conf_mat) > 0)
    return(input.data)
}

append_lateral_vignettes <- function (input.data, lateral_vign) {
    if (is.null(lateral_vign$wdata))
        return(input.data)
    vdata <- lateral_vign$wdata
    vdata <- vdata + 1
    input.data$wdata %<>% full_rbind(vdata)
    # We actually don't need vignette reported confidences.
    # The reported confidences get cut off later.
    vdata[!is.na(vdata)] <- 1
    input.data$conf_mat %<>% full_rbind(vdata)
    stopifnot(nrow(input.data$wdata) > 0, nrow(input.data$conf_mat) > 0)
    return(input.data)
}


main_country <- function(wdata, coder_table) {
    coders <- colnames(wdata)
    cdata <- coder_table$main_country_id[match(coders, coder_table$coder_id)]

    names(cdata) <- coders

    stopifnot(`We did not find the main country for every coder!` = 
        !any(is.na(cdata)))
    return(cdata)
}

create_historical_new_coder_matrices <- function(wdata, coder_table) {
    vignettes_b <- rownames(wdata) %>% is_vignette
    countries <- rownames(wdata)[!vignettes_b] %>% get_text_id

    ll <- split.data.frame(wdata[!vignettes_b, ],
                      factor(countries, levels = unique(countries)))

    # Avoid filtering question_id and coder_id each iteration of lapply
    var_coder_table <-
        coder_table %>%
        filter(coder_id %in% colnames(wdata))

    ml <- lapply(ll, function(m) {
        country <- get_text_id(rownames(m)) %>% first

        coders.df <- filter(var_coder_table, country_text_id == country)

        # This is weird code, but we need logical matrices telling us when
        # a coder-country-date (i.e. each observation in the matrix) is
        # lateral, historical, or new_coder. Remember, new_coder are
        # coders whose ratings start >2005.
        historical <- imprint(m, colnames(m) %in% coders.df$coder_id[coders.df$historical])
        new_coder <- imprint(m, colnames(m) %in% coders.df$coder_id[coders.df$new_coder])
        # set new_coder to FALSE for sequential coders!

        # we set lateral as false later because we recoded all values as vignettes
        list(historical = historical, new_coder = new_coder)
    })

    return(ml)
}

add_matrix_empty_rows <- function(input.data) {
    stopifnot(is.list(input.data))
    stopifnot(all(c("wdata", "historical", "new_coder") %in% names(input.data)))
    if (!any(is_vignette(rownames(input.data$wdata))))
        return(input.data)

    vignettes <- rownames(input.data$wdata)[is_vignette(rownames(input.data$wdata))]
    dummy <- matrix(FALSE, length(vignettes), ncol(input.data$wdata),
                        dimnames = list(vignettes, colnames(input.data$wdata)))
    input.data$historical %<>% rbind(dummy)
    input.data$new_coder %<>% rbind(dummy)
    return(input.data)
}

pre_check_and_sorting <- function(input.data) {
    sort_matrix <- function(m) m[sort_text_id(rownames(m)), as.character(sort(as.numeric(colnames(m))))]

    input.data$wdata %<>% sort_matrix
    input.data$conf_mat %<>% sort_matrix
    input.data$historical %<>% sort_matrix
    input.data$new_coder %<>% sort_matrix

    input.data$cdata <- input.data$cdata[match(colnames(input.data$wdata), names(input.data$cdata))]

    # Check if we have any weird mismatches
    equal_names <- function(x, y) identical(dimnames(x), dimnames(y))
    with(input.data, stopifnot(equal_names(wdata, conf_mat), 
        equal_names(wdata, historical),
        equal_names(wdata, new_coder),
        identical(colnames(wdata), names(cdata))))
    return(input.data)
}

calc_missing <- function(qtable, VARNAME, country_dates, utable_names) {
    # This missingness basically only occurs if no coders coded this country-year
    els <- filter(qtable, name == VARNAME) %>%
        pull(date_specific) %>%
        # among the mm vars, we usually have "Election-specific dates." (V-Party) and "Election-specific dates (v2eltype)" (regular V-Dem data)
        grepl("Election-specific dates[.]|Election-specific dates \\(v2eltype\\)[.]", x = .)

    if (!els) {
        full_missing_dates <- setdiff(utable_names, country_dates) %>% sort

        missing <- split(full_missing_dates, get_text_id(full_missing_dates)) %>%
            lapply(function(v) {
                idx <- get_date(v) %>% create_idx
                split(v, idx) %>% vapply(first, character(1))
            }) %>% unlist

        names(missing) <- NULL
    } else {
        missing <- NULL
    }
    return(missing)
}

final_checks <- function(input.data, utable) {
    stopifnot(identical(
    input.data$wdata[!is_vignette(rownames(input.data$wdata)), ],
    clean_by_utable(input.data$wdata[!is_vignette(rownames(input.data$wdata)), ],
                    utable)))
    with(input.data, {
        stopifnot(!is.na(K), !is.na(C))
        stopifnot(C == length(unique(input.data$cdata)))
        stopifnot(max(wdata, na.rm = T) <= K)
        stopifnot(is.na(conf_mat) | between(conf_mat, 0, 1))

        stopifnot(identical(!is.na(wdata), !is.na(conf_mat)))

        stopifnot(all(colSums(!is.na(wdata)) != 0))
        stopifnot(all(rowSums(!is.na(wdata)) != 0))

        stopifnot(!duplicated(rownames(wdata)))

        stopifnot(identical(dim(wdata), dim(conf_mat)),
                  identical(dim(wdata), dim(historical)),
                  identical(dim(wdata), dim(new_coder)))

        stopifnot(identical(dimnames(wdata), dimnames(conf_mat)),
                  identical(dimnames(wdata), dimnames(historical)),
                  identical(dimnames(wdata), dimnames(new_coder)))

        stopifnot(!is.na(historical), !is.na(new_coder))
        stopifnot(nrow(wdata) == N, ncol(wdata) == J)

        stopifnot(ncol(wdata) == length(input.data$cdata),
            !is.na(input.data$cdata),
            !any(!colnames(wdata) %in% names(input.data$cdata)),
            identical(names(input.data$cdata), colnames(wdata)))
        stopifnot(!is.null(country_dates), !is.na(country_dates))
    })
}

normal_vignette_rows <- function(wdata) {
    normal_vignettes.b <- is_vignette(rownames(wdata)) &
        !is_country_date(gsub("^A_", "", rownames(wdata)))
}

lateral_vignette_rows <- function(wdata) {
    lateral_vignettes.b <- is_vignette(rownames(wdata)) &
        is_country_date(gsub("^A_", "", rownames(wdata)))
}

make_priors <- function(input.data, normal_vignettes.b, lateral_vignettes.b, VARNAME) {
    stopifnot(is.list(input.data),
        all(c("wdata", "conf_mat", "historical", "new_coder", "K") %in% names(input.data)))

    vignettes.b <- normal_vignettes.b | lateral_vignettes.b
    # Matrices for generating priors
    wdata       <- input.data$wdata[!vignettes.b, ]
    conf_mat    <- input.data$conf_mat[!vignettes.b, ]

    historical  <- input.data$historical[!vignettes.b, ]
    new_coder   <- input.data$new_coder[!vignettes.b, ]

    ###
    # If we have a v3.* variables, no need for offsets; otherwise, apply
    # offsets based on per mean difference between normal contemporary and
    # new/historical per country.
    if (substring(VARNAME, 1, 2) == "v2") {
        countries <- rownames(wdata) %>% get_text_id

        wdata.split <- split.data.frame(wdata, countries)
        conf_mat.split <- split.data.frame(conf_mat, countries)

        if (any(new_coder)) {
            new_coder.split <- split.data.frame(new_coder, countries)

            # min = 4 meaning that when generating offsets we only take
            # into account country-dates where there are at least 4 or
            # more of each group (normal and new)
            new_offsets <- Map(offset_diff, wdata.split, new_coder.split, conf_mat.split, min = 4)
            wdata.split <- Map(add_to_matrix, wdata.split, new_offsets, by = new_coder.split)
        }

        if (any(historical)) {
            hist.split <- split.data.frame(historical, countries)

            hist_offsets <- Map(offset_diff, wdata.split, hist.split, conf_mat.split)

            # Apply our offsets from successor states to extinct ones (ex
            # Germany -> Saxony)
            hist_offsets <- lapply(names(hist_offsets), function(s) {
                if (s %in% names(input.data$successor))
                    hist_offsets[[input.data$successor[s]]]
                else
                    hist_offsets[[s]]
            })

            wdata.split <- Map(add_to_matrix, wdata.split, hist_offsets, by = hist.split)
        }

        wdata <- do.call(rbind, wdata.split)
    }

    # Make sure we're not below 1 or above K
    wavg <- pmin(wdata, input.data$K) %>% pmax(1) %>% weighted.rowMeans(conf_mat)

    priors <- vector(mode = "numeric", length = nrow(input.data$wdata))
    # names(priors) <- names(wavg)

    # Normalize! Finally!
    priors[1:nrow(wdata)] <- (wavg - mean(wavg, na.rm = T)) / sd(wavg, na.rm = T)
    names(priors)[1:nrow(wdata)] <- names(wavg)
    return(priors)
}

make_vignette_priors <- function(priors, input.data, normal_vignettes.b, lateral_vignettes.b, VARNAME) {
    if (any(normal_vignettes.b)) {
        v <- rownames(input.data$wdata)[normal_vignettes.b]
        thresholds <- sub(".*?__(\\d)$", "\\1", v) %>% type.convert

        if (length(v) > 1)
            stopifnot(diff(sort(thresholds)) %in% c(0, 1))

        priors[normal_vignettes.b] <- to_seq(thresholds)
        names(priors)[normal_vignettes.b] <- v
    }

    if (any(lateral_vignettes.b)) {
        priors[lateral_vignettes.b] <- 0
        names(priors)[lateral_vignettes.b] <-
            rownames(input.data$wdata)[lateral_vignettes.b]
    }

    return(priors)
}

create_sequential_identifiers <- function(wdata.df, coder_table) {
    # For index access in the stan file we need ordered index sequences
    # representing our coders, countries, and country-dates that can be
    # translated back to the appropriate IDs after.
    # This creates a numeric vector from the factor coder_id called rater_id
    wdata.df %<>%
        left_join(distinct(coder_table, coder_id, main_country_id), by = "coder_id") %>%
        mutate(
            rater_id = as.numeric(as.factor(coder_id)),
            main_country_id_seq = as.numeric(as.factor(main_country_id)),
            country_date_id = as.numeric(as.factor(country_date)))

    return(wdata.df)
}

matrix_to_wide_df <- function(input.data) {

    wdata <- input.data$wdata
    ###
    # Transform wdata to long format and remove missing values
    stopifnot(sapply(dimnames(wdata), Negate(is.null)))

    # For debugging purposes
    wdata <- wdata[sort(rownames(wdata)), ]
    wdata <- wdata[, as.character(sort(as.numeric(colnames(wdata))))]

    # Transform our matrix with country-dates and rownames and coder_ids as columns
    # to a long form data.frame with country_date and coder_id
    wdata.df <- expand.grid(dimnames(wdata), 
        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) %>%
        setNames(c("country_date", "coder_id")) %>%
        mutate(code = as.vector(wdata),
            coder_id = as.numeric(coder_id)) %>%
        filter(!is.na(code)) %>%
        arrange(country_date)

    return(wdata.df)
}

main <- function(input.data, coder_table, vig_data, lateral_vign, ctable, 
                 qtable, utable, vtable) {
    info("Appending " %^% VARNAME)

    # Get number of answer categories
    input.data$K <- qtable %>% filter(name == VARNAME) %$% k
    stopifnot(!is.null(input.data$K), !is.na(input.data$K))

    # Successor states! We need them when creating our offset priors
    input.data$successor <- successor_country(ctable)

    # Vignettes! Append the following:
    # lateral as vignettes; real vignettes
    vignette_ids <- vignette_id(vtable, VARNAME)
    input.data <- append_vignettes(input.data, vig_data, vignette_ids, VARNAME)
    input.data <- append_lateral_vignettes(input.data, lateral_vign)

    # Let's find some cdata (main country coded per coder)
    input.data$cdata <- main_country(input.data$wdata, coder_table)
    input.data$C <- length(unique(input.data$cdata))

    # Now lets determine which of our coders are historical, lateral, or a
    # new coder. This information should already be set in our coder_table.
    additional_matrices <- create_historical_new_coder_matrices(input.data$wdata, coder_table)
    input.data$historical <- do.call(rbind, lapply(additional_matrices, `[[`, "historical"))
    input.data$new_coder <- do.call(rbind, lapply(additional_matrices, `[[`, "new_coder"))
    
    # Add empty rows for the historical and new_coder matrices
    input.data <- add_matrix_empty_rows(input.data)

    # Some intermediate checks
    input.data <- pre_check_and_sorting(input.data)

    # Further model parameters
    input.data$N <- nrow(input.data$wdata)  
    input.data$J <- ncol(input.data$wdata)

    # We also want a vector keeping track of missing country-dates so
    # that we can preserve missingness in the z.sample files for the BFAs
    # without having to expand out to the full `country_dates`.
    # BUT, we only want the start date for a sequential series of missing
    # country-dates since we don't want to increase the size of our
    # z.samples too much. AND, this should only be done for non-ELS
    # variables, otherwise for election vars save only dates that we
    # deleted because of lateral coders.
    utable_names <- with(utable, paste(country_text_id, historical_date))
    input.data$missing <- calc_missing(qtable, VARNAME, input.data$country_dates, utable_names)
    
    # Some intermediate checks
    final_checks(input.data, utable)

    # Prepare priors
    normal_vignettes.b <- normal_vignette_rows(input.data$wdata)
    lateral_vignettes.b <- lateral_vignette_rows(input.data$wdata)
    priors <- make_priors(input.data, normal_vignettes.b, lateral_vignettes.b, VARNAME)
    priors <- make_vignette_priors(priors, input.data, normal_vignettes.b, lateral_vignettes.b, VARNAME)
    
    # Transform wdata to data.frame with factors
    # country_date_id is a factor from country_date
    # rater_id is a factor from coder_id
    # cdata is a factor from main_country_id   
    wdata.df <- matrix_to_wide_df(input.data)
    wdata.df <- create_sequential_identifiers(wdata.df, coder_table)

    # Order priors to match index order of country-dates
    # priors <- priors[levels(wdata.df$country_date)]
    stopifnot(!anyNA(priors))
    prior_df <- data.frame(country_date = names(priors), 
        prior = priors, stringsAsFactors = FALSE)

    # Let's do some final (redundant) checks.
    stopifnot(!is.na(wdata.df))
    stopifnot(input.data$J == length(unique(wdata.df$coder_id)))
    stopifnot(input.data$N == length(unique(wdata.df$country_date)))
    stopifnot(nrow(wdata.df) == length(input.data$wdata[!is.na(input.data$wdata)]))
    # stopifnot(as.character(wdata.df$country_date) == names(priors)[wdata.df$country_date_id])
    stopifnot(input.data$cdata_id[wdata.df$rater_id] == as.factor(wdata.df$cdata) %>% as.numeric)
    
    # Sorted by rater_id
    coder_translation <- wdata.df %>% 
        distinct(rater_id, coder_id, main_country_id, main_country_id_seq) %>% 
        select(rater_id, coder_id, main_country_id, main_country_id_seq) %>%
        arrange(rater_id)

    # Sorted by main_country_id_seq
    main_country_translation <- coder_translation %>%
        distinct(main_country_id, main_country_id_seq) %>%
        arrange(main_country_id_seq)

    # Sorted by country_date_id
    country_date_translation <- wdata.df %>%
        distinct(country_date, country_date_id) %>%
        left_join(prior_df, by = "country_date") %>%
        arrange(country_date_id)

    long_data <- wdata.df %>% select(country_date_id, rater_id, code)

    model_input <- list(
        K = input.data$K,
        J = input.data$J,
        C = input.data$C,
        N = input.data$N,
        n_obs = nrow(long_data),
        cdata_id = coder_translation$main_country_id_seq,
        code = long_data$code,
        rater_id = long_data$rater_id,
        country_date_id = long_data$country_date_id,
        mc = country_date_translation$prior)

    return(list(utable = utable, 
        model_input = model_input, 
        country_date_translation = country_date_translation, 
        coder_translation = coder_translation,
        country_dates = input.data$country_dates,
        missing = input.data$missing,
        main_country_translation = main_country_translation))
}


# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    get_globals()

    # Imports
    objs <- find_dep_files(TASK_ID, DB)
    input.data <- objs[[VARNAME %^% "_reduce"]][[VARNAME]]
    coder_table <- objs[[VARNAME %^% "_coder_table"]][[VARNAME]]
    vig_data <- objs[["vignettes_clean_vignettes"]][["vignettes"]]
    lateral_vign <- objs[[VARNAME %^% "_reduce"]][[VARNAME]]$lateral_as_vignettes    
    stopifnot(!is.null(rownames(input.data$wdata)))

    # Reference tables
    ctable <- load_country()
    qtable <- load_qtable()
    utable <- load_country_unit()
    vtable <- read_file(file.path(Sys.getenv("ROOT_DIR"), "refs", "vignette_table.rds"))

    collectedInput <- named_list(input.data, coder_table, vig_data, lateral_vign, 
        ctable, qtable, utable, vtable)
    rm(list = names(collectedInput))
    # Run
    setNames(list(do.call(main, collectedInput)), VARNAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
} else {
    testthat::test_file("~/proj/mm-prep/tests/mm_prep/test_append.R") %>%
		as.data.frame %$% stopifnot(failed == 0L)
}
update_task_status(db = DB)