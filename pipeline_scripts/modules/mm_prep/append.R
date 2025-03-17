#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# The purpose of this script is to create the input file to model.R. That is,
# this is the last "local" step we need to take. The input to this script is:
# reduced scores, vignettes, lateral vignettes.

# The script then creates empirical priors for the country-dates scores and incorporates
# vignettes in the data. The script also creates indices that Stan can use for
# indexing observations.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# get all vignette_id that related to TASK_NAME
vignette_id <- function(vtable, TASK_NAME) {

    stopifnot(!anyNA(vtable$vignette_id))
    stopifnot(!anyNA(vtable$parent_name))
    stopifnot(`TASK_NAME is empty` = TASK_NAME != "")

    # The rule is that each question have several vignettes: |vid| > 1 
    vid <- vtable$vignette_id[vtable$parent_name == TASK_NAME]

    stopifnot(`vignette_id has missingness` = !anyNA(vid))
    return(vid)
}

# get successor country: is used to make offsets for historical coders
# For example, the successor country to Saxony is Germany
successor_country <- function(ctable) {

    predecessor <- ctable[!is.na(ctable$parent_country_id), ]
    predecessor[["successor_text_id"]] <- 
        to_ctext_ids(v = predecessor$parent_country_id, ttable = ctable)

    # Successor as name and predecessor as value
    successor_text_id <-
        with(predecessor, setNames(successor_text_id, country_text_id))
    
    return(successor_text_id)
}

# Turn vignettes into "data points"
append_vignettes <- function(input.data, vig_data, vignette_ids, TASK_NAME) {
    
    stopifnot(`input.data is not a list` = class(input.data) == "list")
    stopifnot(`wdata or conf_mat are missing from input.data`=
        c("wdata", "conf_mat") %in% names(input.data))
    stopifnot(`input.data[["wdata"]] is not a matrix` = is.matrix(
        input.data[["wdata"]]))
    stopifnot(`wdata must have rownames` =
        !is.null(rownames(input.data[["wdata"]])))
    stopifnot(`input.data[["conf_mat"]] is not a matrix` = is.matrix(
        input.data[["conf_mat"]]))
    stopifnot(`conf_mat must have rownames` =
        !is.null(rownames(input.data[["conf_mat"]])))
    stopifnot(`vig_data is not a data.frame` =
        class(vig_data) == "data.frame")

    # Subset on vignette_id and coder_id
    if (length(vignette_ids) > 0) {
        vignettes.df <- vig_data[with(vig_data,
            (parent_name == TASK_NAME & !is.na(parent_name)) &
            coder_id %in% colnames(input.data[["wdata"]])),,drop = FALSE]
        info(sprintf("Found %s-vignettes for %d unique coders",
            TASK_NAME, length(unique(vignettes.df$coder_id))))

        # Duplicated vignettes are not allowed here:
        stopifnot(!any(duplicated(vignettes.df[, c("coder_id", "id")])))

        # If there are actual vignette data in the cleaned vignette data table:
        if (nrow(vignettes.df) > 0) {
            vignettes.df <- organiseRows(vignettes.df, coder_id, vignette_name)
            vignettes.df <- reshape(
                vignettes.df[, c("id", "coder_id", "code")],
                direction = "wide",
                idvar = "id",
                timevar = "coder_id")
            colnames(vignettes.df) <- gsub("code.", "", colnames(vignettes.df), fixed = TRUE)

            # Create vignettes matrix and make rownames into a unique string per
            # vignette: ex. A_*, B_*
            vdata <- data.matrix(vignettes.df[, -1])
            rownames(vdata) <- vignettes.df$id

            # Data needs to be 1-indexed for Stan
            vdata <- vdata + 1

            # Append vdata to wdata as additional rows
            # -- full_rbind matches by colnames
            stopifnot(!is.null(colnames(vdata)))   
            stopifnot(!is.null(colnames(input.data$wdata)))
            stopifnot(all(colnames(vdata) %in% colnames(input.data$wdata))) 
            input.data$wdata <- full_rbind(input.data$wdata, vdata)

            # Sort by colnames
            input.data$wdata <-
                input.data$wdata[, as.character(sort(as.numeric(colnames(input.data$wdata))))]

            # Confidences for vignettes that are not missing are set to 1.
            # Append vdata as rows to conf_mat. These added rows are removed later.
            vdata[!is.na(vdata)] <- 1
            input.data$conf_mat <- full_rbind(input.data$conf_mat, vdata)

            # Sort by colnames
            input.data$conf_mat <-
                input.data$conf_mat[, as.character(sort(as.numeric(colnames(input.data$conf_mat))))]

        }
    }

    stopifnot(nrow(input.data$wdata) > 0, nrow(input.data$conf_mat) > 0)
    return(input.data)
}

# append lateral vignettes
append_lateral_vignettes <- function (input.data, lateral_vign) {
    
    # Return input.data if there are no lateral vignettes
    if (is.null(lateral_vign$wdata)) {
        return(input.data)
    }

    # Same procedure as in append_vignettes for wdata
    vdata <- lateral_vign$wdata + 1
    input.data$wdata <- full_rbind(input.data$wdata, vdata)

    # Same procedure as in append_vignettes for vdata
    vdata[!is.na(vdata)] <- 1
    input.data$conf_mat <- full_rbind(input.data$conf_mat, vdata)
    stopifnot(nrow(input.data$wdata) > 0, nrow(input.data$conf_mat) > 0)
    return(input.data)
}

# get vector of main_country_id with coder_id as names
main_country <- function(wdata, coder_table) {
    
    coders <- colnames(wdata)
    # grab the main_country_id from coder_table. There can be more than one 
    # row per coder, but the column main_country_id is constant within each coder_id.
    cdata <- coder_table$main_country_id[match(coders, coder_table$coder_id)]

    # make cdata a named vector
    names(cdata) <- coders

    stopifnot(`We did not find the main country for every coder!` = 
        !any(is.na(cdata)))

    return(cdata)
}

# Matrices for historical and new coders
create_historical_new_coder_matrices <- function(wdata, coder_table) {

    # bool for vignette    
    vignettes_bool <- is_vignette(rownames(wdata))
    # text_id for all countries in wdata
    countries <- get_text_id(rownames(wdata)[!vignettes_bool])

    ll <- split.data.frame(wdata[!vignettes_bool, ],
        factor(countries, levels = unique(countries)))

    # Avoid filtering question_id and coder_id each iteration of lapply
    var_coder_table <-
        coder_table[coder_table$coder_id %in% colnames(wdata), ]

    ml <- lapply(ll, function(m, vcd) {
        
        country <- get_text_id(rownames(m))[1]
        coders.df <- vcd[vcd$country_text_id == country & !is.na(vcd$country_text_id), ]

        historical <- imprint(m, colnames(m) %in% coders.df$coder_id[coders.df$historical])
        new_coder <- imprint(m, colnames(m) %in% coders.df$coder_id[coders.df$new_coder])

        # we set lateral as false later because we recoded all values as vignettes
        list(historical = historical, new_coder = new_coder)
    }, vcd = var_coder_table)

    return(ml)
}

# add empty rows to make sure that wdata and historical/new_coder matrices 
# have the same dimensions
add_matrix_empty_rows <- function(input.data) {
    stopifnot(is.list(input.data))
    stopifnot(all(c("wdata", "historical", "new_coder") %in% names(input.data)))
    if (!any(is_vignette(rownames(input.data$wdata))))
        return(input.data)

    # Pull out vignettes
    vignettes <- rownames(input.data$wdata)[is_vignette(rownames(input.data$wdata))]
    # Create matrix with only FALSE: rownames are vignette names and colnames are
    # coder_id
    dummy <- matrix(FALSE, length(vignettes), ncol(input.data$wdata),
        dimnames = list(vignettes, colnames(input.data$wdata)))
    
    # Append rows 
    input.data$historical <- rbind(input.data$historical, dummy)
    input.data$new_coder <- rbind(input.data$new_coder, dummy)

    return(input.data)
}


pre_check_and_sorting <- function(input.data) {
    
    sort_matrix <- function(m) {
        m[sort_text_id(rownames(m)),
        as.character(sort(as.numeric(colnames(m))))]
    }

    input.data$wdata %<>% sort_matrix
    input.data$conf_mat %<>% sort_matrix
    input.data$historical %<>% sort_matrix
    input.data$new_coder %<>% sort_matrix

    # Ensures main_country_id matches coders in wdata
    input.data$cdata <-
        input.data$cdata[match(colnames(input.data$wdata), names(input.data$cdata))]

    # Check if we have any weird mismatches
    equal_names <- function(x, y) identical(dimnames(x), dimnames(y))
    with(input.data, stopifnot(equal_names(wdata, conf_mat), 
        equal_names(wdata, historical),
        equal_names(wdata, new_coder),
        identical(colnames(wdata), names(cdata))))
    return(input.data)
}

# Returns a vector of years that exists but are missing
identify_coding_gap_years <- function(country_dates, utable) {
	
    # country-years from country-unit (CU)
    utable_country_years <- 
        sort(paste(utable$country_text_id, utable$year, sep = " "))
    # country-years from input data (DA)
    v_country_years <-
        sort(unique(substr(country_dates, 1, 8)))
    # CA / DA <-> x \in (CA \cap DA^C)
	full_missing_years <-
        sort(setdiff(utable_country_years, v_country_years))
    # Add January first as well
	full_missing_years <- paste0(full_missing_years, "-01-01")

	# Get first of each sequence of years
    # We use this to identify years where in the z-sample files from the mm
    # we do not want to stretch across for the BFAs (see the model.R script).
	missing <-
        split(full_missing_years, get_text_id(full_missing_years)) |> 
        lapply(X =_, function(v) {
            idx <- create_idx(get_date(v))
            out <- vapply(split(v, idx), function(x) x[1], character(1))
            return(out)
            }) |> 
        unlist() |>  
        unname()

    return(missing)
}

# checks: this is the last step before we can run submit_mm, hence the many checks
final_checks <- function(input.data, utable) {
    stopifnot(identical(
    input.data$wdata[!is_vignette(rownames(input.data$wdata)), ],
    clean_by_utable(input.data$wdata[!is_vignette(rownames(input.data$wdata)), ],
                    utable)))
    with(input.data, {
        stopifnot(!is.na(K), !is.na(C))
        stopifnot(C == length(unique(input.data$cdata)))
        stopifnot(max(wdata, na.rm = T) <= K)
        stopifnot(
            is.na(conf_mat) | 
            (conf_mat >= 0 & !is.na(conf_mat) & conf_mat <= 1)
            )

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

# normal vignettes are named A_
normal_vignette_rows <- function(wdata) {
    normal_vignettes.lgl <- is_vignette(rownames(wdata)) &
        !is_country_date(gsub("^A_", "", rownames(wdata)))

    return(normal_vignettes.lgl)
}

# lateral vignettes are named AFG, MEX
lateral_vignette_rows <- function(wdata) {
    lateral_vignettes.lgl <- is_vignette(rownames(wdata)) &
        is_country_date(gsub("^A_", "", rownames(wdata)))

    return(lateral_vignettes.lgl)
}

make_priors <- function(input.data, normal_vignettes.b, lateral_vignettes.b, TASK_NAME) {
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
    if (substring(TASK_NAME, 1, 2) == "v2") {
        countries <- get_text_id(rownames(wdata))

        wdata.split <- split.data.frame(wdata, countries)
        conf_mat.split <- split.data.frame(conf_mat, countries)

        if (any(new_coder)) {

            # One offset per country
            new_coder.split <- split.data.frame(new_coder, countries)
            # min = 4 meaning that when generating offsets we only take
            # into account country-dates where there are at least 4 or
            # more of each group (normal and new)
            new_offsets <- Map(offset_diff, wdata.split, new_coder.split, conf_mat.split, min = 4)
            wdata.split <- Map(add_to_matrix, wdata.split, new_offsets, by = new_coder.split)
        }

        if (any(historical)) {

            # One offset per country in countries
            hist.split <- split.data.frame(historical, countries)
            # Note that min defaults to 1 when calculating offset_diff
            hist_offsets <- Map(offset_diff, wdata.split, hist.split, conf_mat.split)

            # Apply our offsets from successor states to extinct ones (ex
            # Germany -> Saxony)
            # There is no overlapping period between extinct countries and 
            # contemporary successor.
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
    wavg <- pmax(pmin(wdata, input.data$K), 1) |> 
        weighted.rowMeans(x=_, y=conf_mat)

    # Define empirical priors (look at mm paper for specifics)
    priors <- (wavg - mean(wavg, na.rm = T)) / sd(wavg, na.rm = T)
    names(priors) <- names(wavg)

    return(priors)
}

make_vignette_priors <- function(priors, input.data, normal_vignettes.b, lateral_vignettes.b, TASK_NAME) {
    
    if (any(normal_vignettes.b)) {
        # vignettes have their threshold-pair after '__'
        v <- rownames(input.data$wdata)[normal_vignettes.b]
        thresholds <- type.convert(sub(".*?__(\\d)$", "\\1", v), as.is = TRUE)

        # we expect consecutive threholds
        if (length(v) > 1)
            stopifnot(diff(sort(thresholds)) %in% c(0, 1))

        # to_seq creates a sequence of z-scores from -1.5 to 1.5
        priors[normal_vignettes.b] <- to_seq(thresholds)
        names(priors)[normal_vignettes.b] <- v
    }

    # Lateral vignettes does not have values per threshold
    if (any(lateral_vignettes.b)) {
        # m_wdata <- input.data$wdata[lateral_vignettes.b, ]
        # m_conf <- input.data$conf[lateral_vignettes.b, ]
        
        # lateral vignettes have hardcoded z-score of 0
        priors[lateral_vignettes.b] <- 0
        names(priors)[lateral_vignettes.b] <-
            rownames(input.data$wdata)[lateral_vignettes.b]
    }

    return(priors)
}

create_sequential_identifiers <- function(wdata.df, coder_table) {

    wdata.df <- merge(
        x = wdata.df, y = dplyr::distinct(coder_table, coder_id, main_country_id),
        by = "coder_id", all.x = TRUE)
    
    wdata.df <- within(wdata.df, {
        rater_id <- as.numeric(as.factor(coder_id))
        main_country_id_seq <- as.numeric(as.factor(main_country_id))
        country_date_id <- as.numeric(as.factor(country_date))
    })

    return(wdata.df)
}

matrix_to_wide_df <- function(input.data) {

    wdata <- input.data$wdata
    # Transform wdata to long format and remove missing values
    stopifnot(sapply(dimnames(wdata), Negate(is.null)))

    # For debugging purposes
    wdata <- wdata[sort(rownames(wdata)), ]
    wdata <- wdata[, as.character(sort(as.numeric(colnames(wdata))))]

    # Transform our matrix with country-dates and rownames and coder_ids as columns
    # to a long form data.frame with country_date and coder_id
    wdata.df <- expand.grid(dimnames(wdata), 
        KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE) |> 
        setNames(c("country_date", "coder_id")) |> 
        mutate(code = as.vector(wdata),
            coder_id = as.numeric(coder_id)) |> 
        filter(!is.na(code)) |> 
        arrange(country_date)

    return(wdata.df)
}

main <- function(input.data, coder_table, vig_data, lateral_vign, ctable, qtable, utable, vtable, elecreg, TASK_NAME) {
    
    stopifnot(`qtable contains more than one entry` = nrow(qtable) == 1)
    info(sprintf("Appending %s", TASK_NAME))

    # Get number of answer categories
    input.data$K <- qtable$k[qtable$name == TASK_NAME]
    stopifnot(!is.null(input.data$K), !is.na(input.data$K))

    # Successor states: needed to create our offset priors
    input.data$successor <- successor_country(ctable)

    # Vignettes, append the following:
    # 1) lateral as vignettes and 2) real vignettes
    input.data <- append_vignettes(
        input.data,
        vig_data,
        vignette_id(vtable, TASK_NAME),
        TASK_NAME)
    input.data <- append_lateral_vignettes(input.data, lateral_vign)

    # Main country coded for each coder: used to cluster thresholds
    input.data$cdata <- main_country(input.data$wdata, coder_table)
    # Number of unique (main) countries
    input.data$C <- length(unique(input.data$cdata))

    # Now lets determine which of our coders are historical, lateral, or a
    # new coder. This information should already be set in our coder_table.
    additional_matrices <- create_historical_new_coder_matrices(input.data$wdata, coder_table)
    input.data$historical <- do.call(rbind, lapply(additional_matrices, `[[`, "historical"))
    input.data$new_coder <- do.call(rbind, lapply(additional_matrices, `[[`, "new_coder"))
    
    # Add empty rows for the historical and new_coder matrices
    input.data <- add_matrix_empty_rows(input.data)

    input.data <- pre_check_and_sorting(input.data)

    # Further model parameters:
    # 1) (N)number of observations, 2) number of coders (J)
    input.data$N <- nrow(input.data$wdata)  
    input.data$J <- ncol(input.data$wdata)

    if (!isTRUE(qtable$els)) {
        input.data$missing <- identify_coding_gap_years(
            input.data$country_dates, utable)
    }

    final_checks(input.data, utable)

    # Prepare priors
    normal_vignettes.b <- normal_vignette_rows(input.data$wdata)
    lateral_vignettes.b <- lateral_vignette_rows(input.data$wdata)
    priors <- make_priors(input.data, normal_vignettes.b, lateral_vignettes.b, TASK_NAME)
    priors <- make_vignette_priors(priors, input.data, normal_vignettes.b, lateral_vignettes.b, TASK_NAME)

    # Create iterators for Stan
    wdata.df <- create_sequential_identifiers(matrix_to_wide_df(input.data), coder_table)

    stopifnot(!anyNA(priors))
    prior_df <- data.frame(
        country_date = names(priors), 
        prior = priors,
        stringsAsFactors = FALSE)

    # Checks
    stopifnot(!is.na(wdata.df))
    stopifnot(input.data$J == length(unique(wdata.df$coder_id)))
    stopifnot(input.data$N == length(unique(wdata.df$country_date)))
    stopifnot(nrow(wdata.df) == length(input.data$wdata[!is.na(input.data$wdata)]))
    stopifnot(input.data$cdata_id[wdata.df$rater_id] == as.factor(wdata.df$cdata) %>% as.numeric())
    
    # Sorted by rater_id
    coder_translation <- wdata.df |> 
        distinct(rater_id, coder_id, main_country_id, main_country_id_seq) |> 
        select(rater_id, coder_id, main_country_id, main_country_id_seq) |> 
        arrange(rater_id)

    # Sorted by main_country_id_seq
    main_country_translation <- coder_translation |> 
        distinct(main_country_id, main_country_id_seq) |> 
        arrange(main_country_id_seq)

    # Sorted by country_date_id
    country_date_translation <- wdata.df |> 
        distinct(country_date, country_date_id) |> 
        left_join(prior_df, by = "country_date") |> 
        arrange(country_date_id)

    long_data <- organiseRows(
        wdata.df[, c("country_date_id", "rater_id", "code")],
        country_date_id, rater_id)

    # Input for measurement model
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

    return(list(
        utable = utable, 
        model_input = model_input, 
        country_date_translation = country_date_translation, 
        coder_translation = coder_translation,
        country_dates = input.data$country_dates,
        missing = input.data$missing,
        main_country_translation = main_country_translation,
		country = ctable,
		elecreg = elecreg,
		qtable = qtable))
}


# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    objs <- find_dep_files(TASK_ID, db)
    input.data <- objs[[sprintf("%s_reduce", TASK_NAME)]][[TASK_NAME]]
    coder_table <- objs[[sprintf("%s_coder_table", TASK_NAME)]][[TASK_NAME]]
    vig_data <- objs[["vignettes_clean_vignettes"]][["vignettes"]]
    lateral_vign <- objs[[sprintf("%s_reduce", TASK_NAME)]][[TASK_NAME]]$lateral_as_vignettes
    stopifnot(!is.null(rownames(input.data$wdata)))

    # Reference tables
    ctable <- load_country()
    qtable <- subset(load_qtable(), subset = name == TASK_NAME)
    utable <- load_country_unit()
    vtable <- read_file(file.path(Sys.getenv("ROOT_DIR"), "refs", "vignette_table.rds"))

	if (isTRUE(qtable$els[qtable$name == TASK_NAME])) {
		elecreg <- objs[[grep("elecreg", names(objs), value = TRUE)]][[1]]$cy
	} else {
		elecreg <- NULL
	}

    collectedInput <- named_list(input.data, coder_table, vig_data, lateral_vign, 
        ctable, qtable, utable, vtable, elecreg, TASK_NAME)
    rm(list = c("input.data", "coder_table", "vig_data", "lateral_vign", 
        "ctable", "qtable", "utable", "vtable", "elecreg"))
    # Run
    setNames(list(do.call(main, collectedInput)), TASK_NAME) %>%
        write_file(., OUTFILE, dir_create = TRUE)
} else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/mm_prep/test_append.R") %>%
		check_test_file_output()
}
