#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# This script creates the following variables / indices:
# - v2x_elecreg, v2xex_elecreg, v2xlg_elecreg, v2xel_elecparl, v2xel_elecpres, 
#   v2ex_elechos, v2ex_elechog, v2x_hosinter, v2xlg_leginter, v2x_hosabort, v2x_legabort,
#   v2ellocumul, v2elprescumul, v2ellocons, v2elprescons
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()

objs <- find_dep_files(TASK_ID, db)
qtable <- load_qtable()
country <- load_country()
utable <- load_country_unit()

# Functions
# ------------------------------------------------------------------------------
# Function to create v2x_hosabort and v2x_legabort (HOS/legislative election results aborted?) 
# -- The input vars for v2x_hosabort are v2xel_elecpres and v2ex_elechos 
# -- The input vars for v2x_legabort are v2xel_elecparl and v2lgbicam
create_abort <- function(df, var1, var2) {

    if (var1 == "v2xel_elecpres") {
        eltype_round1 <- "v2eltype_6"
        eltype_round2 <- "v2eltype_7"
        other <- "v2xel_elecparl"
    } else if (var1 == "v2xel_elecparl") {
        eltype_round1 <- "v2eltype_0"  
        eltype_round2 <- "v2eltype_1"
        other <- "v2xel_elecpres" 
    }

    diff_year <- c(FALSE, diff(df$historical_date) < 366)

    df <- df |>
        mutate(current_default_date = grepl("12-31$", as.character(historical_date)),
                next_default_date = grepl("12-31$", as.character(lead(historical_date))),
                second_round = case_when(
                    lead(.data[[eltype_round2]]) == 1 | lead(.data[[eltype_round2]], 2) == 1 | lead(.data[[eltype_round2]], 3) == 1 ~ TRUE,
                    TRUE ~ FALSE),
                other_election = case_when(lead(.data[[other]]) == 1 ~ TRUE,
                    TRUE ~ FALSE), 
                follow_other_default = grepl("12-31$", as.character(lead(historical_date, 2))) 
                )
    df <- df |>
        mutate(
            abort = case_when(
                current_default_date == TRUE & second_round == FALSE &
                    .data[[var1]] == 1 & 
                    lead(.data[[var2]], 2) == 0 & lead(.data[[var2]], 3) == 0 &
                    diff_year ~ TRUE,
                next_default_date == TRUE & second_round == FALSE &
                    .data[[var1]] == 1 & 
                    lead(.data[[var2]], 3) == 0 & lead(.data[[var2]], 4) == 0 &
                    diff_year ~ TRUE,
                next_default_date == FALSE & current_default_date == FALSE & 
                    second_round == FALSE & other_election == FALSE &
                    .data[[var1]] == 1 & 
                    lead(.data[[var2]]) == 0 & lead(.data[[var2]], 2) == 0 &
                    diff_year ~ TRUE,
                other_election == TRUE & follow_other_default == FALSE & 
                    second_round == FALSE &
                    .data[[var1]] == 1 & 
                    lead(.data[[var2]], 2) == 0 & lead(.data[[var2]], 3) == 0 &
                    diff_year ~ TRUE,
                other_election == TRUE & follow_other_default == TRUE & 
                    second_round == FALSE &
                    .data[[var1]] == 1 & 
                    lead(.data[[var2]], 4) == 0 & lead(.data[[var2]], 5) == 0 &
                    diff_year ~ TRUE,

                TRUE ~ FALSE))

    is.na(df$abort) <- is.na(df[[var2]])

    df <- df |>
        mutate(
            abort = case_when(
                lead(abort) == TRUE & lead(.data[[eltype_round2]]) == 1 & .data[[eltype_round1]] == 1 ~ TRUE,
                lead(abort, 3) == TRUE & lead(.data[[eltype_round2]], 3) == 1 & .data[[eltype_round1]] == 1 ~ TRUE,
                TRUE ~ abort
            )
        )
    
    if(var1 == "v2xel_elecpres") {
        df$abort[(df$v2ex_elechog == TRUE) | (lead(df$v2ex_elechog) == TRUE) |
            (lead(df$v2ex_elechog, 2) == TRUE & df$abort == TRUE)] <- FALSE
    }

    return(df$abort)
}

# If HOS/legislature is back within 366 days it does not count as aborted
# -- Few hardcoded cases for hosabort
clean_abort <- function(df, var, country) {
    for (i in which(df[[var]])) {
        if (var == "v2x_hosabort") {
        condition <- df$v2ex_elechos[(i + 1):length(df$v2ex_elechos)] &
            df$historical_date[(i + 1):length(df$historical_date)] -
            df$historical_date[i] < 366
    
        } else if (var == "v2x_legabort") {
        condition <- df$v2lgbicam[(i + 1):length(df$v2lgbicam)] >= 1 &
            df$historical_date[(i + 1):length(df$historical_date)] -
            df$historical_date[i] < 366
        }

        if (any(condition)) {
            df[[var]][i] <- FALSE
        } else {
            df[[var]][i] <- TRUE
        }
    }

    if (var == "v2x_hosabort") {
        if (country == "BOL") {
            df[[var]][between(df$year, 1985, 2002)] <- FALSE
        } else if (country == "EST") {
            df[[var]][df$year == 1992] <- FALSE
        } else if (country == "CRI") {
            df[[var]][df$year == 1913 | df$year == 1919 | df$year == 1923 | df$year == 1932] <- FALSE
        } else if (country == "IRN") {
            df[[var]][between(df$year, 1981, 1985)] <- FALSE
            df[[var]][df$historical_date == "1980-01-25"] <- FALSE
        } else if (country == "ARG") {
            df[[var]][df$year == 1973] <- FALSE
        } else if (country == "URY") {
            df[[var]][df$historical_date == "1920-11-28"] <- FALSE
        } else if (country == "ECU") {
            df[[var]][df$historical_date == "1931-10-20"] <- TRUE
        }
    }

    if (var == "v2x_legabort") {
        if (country == "BOL") {
            df[[var]][(df$historical_date == "2019-10-20")] <- TRUE
        } 
    }

    return(df[[var]])
}

create_cumulative_indices <- function(df, var_name, input_var1, input_var2) {
    # prep df
    if (var_name != "v2elprescumul") {    
        df <- df |>
            group_by(country_text_id) |>
            arrange(historical_date) 
    } 
    
    if (var_name %in% c("v2ellocons", "v2elprescons")) {
        if (var_name == "v2ellocons") {
            diff_var <- "v2xlg_elecreg"
        } else {
            diff_var <- "v2xex_elecreg"
        }
        df <- df |>
            mutate(regime = (c(FALSE, diff(.data[[diff_var]]) != 0) |> cumsum())) |>
            group_by(country_text_id, regime) 
    }
    
    # calculate new var
    df <- df |>
        mutate(out_var = cumsum((!is.na(.data[[input_var1]]) & .data[[input_var1]] == 1) |
                                (!is.na(.data[[input_var2]]) & .data[[input_var2]] == 1)))
    
    # remove regime
    if ("regime" %in% colnames(df)) {
        df <- df |>
            ungroup() |> 
            select(-regime)
    }

    # add dynamic var_name
    names(df)[names(df) == "out_var"] <- var_name
    
    return(df)
}

# find aggregation method from qtable
find_aggregation_method <- function(qtable, vars, aggregation_method) {
    # Extract variables based on aggregation method
    out_vars <- qtable |>
        subset(name %in% vars) |>
        subset(cy_aggregation == aggregation_method)
    if (nrow(out_vars) > 0) {
        return(out_vars$name)
    }
}

# shifts to cy format for either last or max
aggregate_to_cy <- function(df, vars, aggregation_method) {
    out_df <- df |>
        select(country_id, country_text_id, year, one_of(vars)) |>
        group_by(country_id, country_text_id, year)

    if (aggregation_method == "last") {
        out_df <- out_df |>
            summarise_all(collect_last)
    } else if (aggregation_method == "max") {
        out_df <- out_df |>
            summarise_all(collect_max)
    }

    return(out_df)
}

# ------------------------------------------------------------------------------
# Reduce all dependencies to one data.frame, and add country_text_id
df_load <-
    lapply(names(objs), function(v) objs[[v]][[v]]$cd) |> 
    Reduce(f = full_join_vdem, x = _) |>  
    merge(x = _, y = country[, c("country_id","country_text_id")],
        by = "country_id", all.x = TRUE) |> 
    organiseRows(country_id, historical_date)

# Subset the data to only include what we use
a_vars_keep <- c(
    "country_text_id", "year", "historical_date", "country_id",
    sort(grep("^v2eltype", names(df_load), value = TRUE)),
    "v2expathhs", "v2expathhg", "v2exhoshog", "v2lgbicam", "v2lgello")
a_vars_df <-
    organiseRows(df_load[, a_vars_keep], country_text_id, historical_date) |> 
    merge(x = _, y = utable[, c("country_text_id", "year", "gap_idx")],
        by = c("country_text_id", "year"), all.x = TRUE) 

# Interpolate
fill_cols <- c("v2expathhs", "v2expathhg", "v2exhoshog", "v2lgbicam", "v2lgello")
splits <- split(a_vars_df, list(a_vars_df$country_text_id, a_vars_df$year), drop = TRUE)

a_vars_df <- parallel::mclapply(splits, function(dat, FC) {
        dat <- organiseRows(dat, historical_date)
        for (fc in FC) { dat[[fc]] <- locf(dat[[fc]]) }
        return(dat)
    }, FC = fill_cols, mc.cores = 12) |>
    do.call(rbind.data.frame, args = _) |> 
    organiseRows(country_text_id, historical_date) |>
    # interpolate gap_idx
    (\(x) split(x = x, f = x$country_text_id, drop  = TRUE))() |>
    parallel::mclapply(X = _, function(dat) {
        dat <- organiseRows(dat, historical_date, decreasing=TRUE)
        dat[["gap_idx"]] <- locf(dat[["gap_idx"]])
        return(dat)
    }, mc.cores = 12) |> 
    do.call(rbind.data.frame, args = _) |> 
    organiseRows(country_text_id, historical_date) |>
    # interpolate missing values within country_text_id and gap_idx (interpolates across years within the same country_text_id)
    (\(x) split(x = x, f = list(x$country_text_id, x$gap_idx), drop = TRUE))() |>
    parallel::mclapply(X = _, function(dat, FC) {
        dat <- organiseRows(dat, historical_date)
        for (fc in FC) { dat[[fc]] <- locf(dat[[fc]]) }
        return(dat)
    }, FC = fill_cols, mc.cores = 12) |>
    do.call(rbind.data.frame, args = _) |>
    organiseRows(country_text_id, historical_date) 

# Clean after interpolation
a_vars_df <- within(a_vars_df, {
    v2lgello = ifelse(v2lgbicam == 0, NA, v2lgello)
    v2expathhg = ifelse(v2exhoshog == 1, NA, v2expathhg)
})

a_split <- split(
    x=a_vars_df,
    f = list(a_vars_df$gap_idx, a_vars_df$country_text_id),
    drop = TRUE)

ll <- lapply(a_split, function(df) {
    country <- unique(df$country_text_id)
    new_vars <- df |>
        mutate(
            # v2exel_elecparl (legislative or constituent assembly election)
            v2xel_elecparl = if_else(v2eltype_0 == 1 | v2eltype_1 == 1 | v2eltype_4 == 1 | v2eltype_5 == 1, TRUE, FALSE),
            v2xel_elecparl = if_else(is.na(v2xel_elecparl), FALSE, v2xel_elecparl),
            
            # v2exel_elecpres (presidential election)
            v2xel_elecpres = if_else(v2eltype_6 == 1 | v2eltype_7 == 1, TRUE, FALSE),
            v2xel_elecpres = if_else(is.na(v2xel_elecpres), FALSE, v2xel_elecpres),            
            
            # Legislative or presidential election
            elec = if_else(v2xel_elecpres | v2xel_elecparl, TRUE, FALSE),    
            
            # v2ex_elechos (HOS appointed through popular election)
            # Leave as missing if v2expathhg is missing
            v2ex_elechos = if_else(v2expathhs == 7, TRUE, FALSE),
            v2ex_elechos = if_else(country == "PHL" & historical_date > "1985-01-01" & historical_date < "1993-01-01", TRUE, v2ex_elechos),
            v2ex_elechos = if_else(country == "PHL" & historical_date > "2000-01-01" & historical_date < "2005-01-01", TRUE, v2ex_elechos),            
            
            # v2ex_elechog (HOG appointed through popular election)
            # Missing if HOS = HOG
            v2ex_elechog = if_else(v2expathhg == 8, TRUE, FALSE),
            v2ex_elechog = if_else(v2exhoshog != 0, NA, v2ex_elechog))
    
    diff_elechos <- c(NA, diff(new_vars$v2ex_elechos))
    diff_elechog <- c(NA, diff(new_vars$v2ex_elechog))

    new_vars <- new_vars |>
        mutate(
            # v2x_hosinter (HOS no longer elected) Set missing when v2ex_elechos is missing
            v2x_hosinter = if_else(diff_elechos == -1 & !(v2expathhs == 6 & (v2lgbicam %in% c(1, 2))), TRUE, FALSE),
            v2x_hosinter = if_else(is.na(v2ex_elechos), NA, v2x_hosinter),
            v2x_hosinter = if_else(is.na(v2x_hosinter) & !is.na(v2ex_elechos), FALSE, v2x_hosinter),

            # hoginter (HOG no longer elected) 
            # -- Not actually a variable just used to calculate v2xex_elecreg 
            # -- Set to missing then false when v2ex_elechog is missing
            hoginter = if_else(diff_elechog == -1, TRUE, FALSE),
            hoginter = if_else(is.na(v2ex_elechog), NA, hoginter),
            hoginter = if_else(is.na(hoginter) & !is.na(v2ex_elechog), FALSE, hoginter),
            hoginter = if_else(is.na(hoginter), FALSE, hoginter),

            # Clean up v2lgello (Percentage of directly elected officials in lower chamber) 
            # -- used for leginter 
            # -- Set to FALSE for interpolation when no chamber 
            # -- NA values are replaces with the last value
            v2lgello2 = if_else(v2lgbicam == 0 & !is.na(v2lgbicam), 0, v2lgello),
            v2lgello2 = locf(v2lgello2),

            # v2xlg_leginter (Legislature closed down or aborted)
            v2xlg_leginter = if_else((v2lgbicam == 0 & lag(v2lgbicam) > 0) | (v2lgello2 == 0 & lag(v2lgello2) > 0), TRUE, FALSE),
            v2xlg_leginter = if_else(is.na(v2lgello2), FALSE, v2xlg_leginter),
            
            v2x_hosabort = create_abort(new_vars, "v2xel_elecpres", "v2ex_elechos"),
            v2x_legabort = create_abort(new_vars, "v2xel_elecparl", "v2lgbicam"),
            v2x_hosabort = clean_abort(new_vars, "v2x_hosabort", country),
            v2x_legabort = clean_abort(new_vars, "v2x_legabort", country),

            # Electoral regime v2x_elecreg
            # Election makes an electoral regime
            # -- Interruption of election or no longer elected -> FALSE
            v2x_elecreg = v2xel_elecparl | v2xel_elecpres,
            v2x_elecreg = if_else(v2x_elecreg == FALSE, NA, v2x_elecreg),
            v2x_elecreg = case_when(v2x_hosinter | v2xlg_leginter | lag(v2x_hosabort) | lag(v2x_legabort) ~ FALSE, 
                TRUE ~ v2x_elecreg),
            v2x_elecreg = locf(v2x_elecreg),
            v2x_elecreg = if_else(is.na(v2x_elecreg), FALSE, v2x_elecreg),

            # Executive electoral regime index v2xex_elecreg
            v2xex_elecreg = v2xel_elecpres,
            v2xex_elecreg = if_else(v2xex_elecreg == FALSE, NA, v2xex_elecreg),
            v2xex_elecreg = case_when(hoginter | v2x_hosinter | lag(v2x_hosabort) ~ FALSE, 
                TRUE ~ v2xex_elecreg),
            v2xex_elecreg = locf(v2xex_elecreg),
            v2xex_elecreg = if_else(is.na(v2xex_elecreg), FALSE, v2xex_elecreg),

            # Legislative electoral regime index v2xlg_elecreg
            v2xlg_elecreg = v2xel_elecparl,
            v2xlg_elecreg = if_else(v2xlg_elecreg == FALSE, NA, v2xlg_elecreg),
            v2xlg_elecreg = case_when(v2xlg_leginter | lag(v2x_legabort) ~ FALSE, 
                TRUE ~ v2xlg_elecreg),
            v2xlg_elecreg = locf(v2xlg_elecreg),
            v2xlg_elecreg = if_else(is.na(v2xlg_elecreg), FALSE, v2xlg_elecreg)
            )

    out <- new_vars |>
        select(v2x_elecreg, v2xex_elecreg, v2xlg_elecreg, v2xel_elecparl, v2xel_elecpres, v2ex_elechos,
        v2ex_elechog, v2x_hosinter, v2xlg_leginter, v2x_hosabort, v2x_legabort)

    out[] <- lapply(out, as.numeric)
    
    return(out)
})

mc_assert(ll)

elecreg_df <-
    select(
        a_vars_df, country_text_id, country_id, historical_date, year,
        matches("v2eltype")) |> 
    bind_cols(bind_rows(ll)) |> 
    organiseRows(country_id, historical_date)

####
# Override elecreg for countries with left-censoring issue (ie
# codingstart 1900 w/ no hist data, but first elections were in 19th
# century) and Libya & Syria since war.
elecreg_df %<>% mutate(
    v2x_elecreg = case_when(country_text_id == "ZWE" & year %in% c(1900, 1901, 1902) ~ 1,
                            country_text_id == "JAM" & year == 1900 ~ 1,
                            country_text_id == "ISL" & year == 1900 ~ 1,
                            country_text_id == "SYR" & year >= 2013 ~ 0,
                            country_text_id == "LBY" & year >= 2014 ~ 0,
                            TRUE ~ v2x_elecreg),
    v2xex_elecreg = case_when(country_text_id == "SYR" & year >= 2013 ~ 0,
                            T ~ v2xex_elecreg),
    v2xlg_elecreg = case_when(country_text_id == "ZWE" & year %in% c(1900, 1901, 1902) ~ 1,
                            country_text_id == "JAM" & year == 1900 ~ 1,
                            country_text_id == "ISL" & year == 1900 ~ 1,
                            country_text_id == "SYR" & year >= 2013 ~ 0,
                            country_text_id == "LBY" & year >= 2014 ~ 0,
                            T ~ v2xlg_elecreg))

# Let's construct the cumulative election indices. As listed in the codebook, this is from 1900 until present.
# -- Make sure that v2elprescumul directly follows v2ellocumul
final_df <- elecreg_df |>
    filter(historical_date >= '1900-01-01') |>
    # Lower chamber election cumulative
    create_cumulative_indices("v2ellocumul", "v2eltype_0", "v2eltype_1") |>
    # Presidential election cumulative
    create_cumulative_indices("v2elprescumul", "v2eltype_6", "v2eltype_7") |>
    # Consecutive lower chamber elections
    create_cumulative_indices("v2ellocons", "v2eltype_0", "v2eltype_1") |>
    # Consecutive presidential elections
    create_cumulative_indices("v2elprescons", "v2eltype_6", "v2eltype_7") |>
    select(country_text_id, historical_date, v2ellocumul, v2elprescumul, 
        v2ellocons, v2elprescons) |>
    right_join(elecreg_df, by = c("country_text_id", "historical_date")) |>
    select(-matches("v2eltype"))

# Aggregate to country-year format 
cy_df <- select(final_df, country_text_id, country_id, year,
            v2x_elecreg, v2xex_elecreg, v2xlg_elecreg,
            v2xel_elecparl, v2xel_elecpres, v2ex_elechos,
            v2ex_elechog, v2x_hosinter,
            v2ellocumul, v2elprescumul, v2ellocons,
            v2elprescons, v2x_legabort, v2x_hosabort,
            v2xlg_leginter)

# Get aggregation method from qtable
vars <- setdiff(names(cy_df), c("country_text_id", "country_id", "year"))

last_vars <- find_aggregation_method(qtable, vars, "Last")
max_vars <- find_aggregation_method(qtable, vars, "Maximum")

# Aggregate to country-year format
last_df <- aggregate_to_cy(cy_df, last_vars, "last")
max_df <- aggregate_to_cy(cy_df, max_vars, "max")

final_cy_df <- full_join(
        last_df, max_df,
        by = c("country_id", "country_text_id", "year")) |>
	ungroup()
stopifnot(colnames(cy_df) %in% colnames(final_cy_df))

# Clean by utable
final_df <- final_df |> 
    inner_join(select(utable, country_text_id, year),
        by = c("country_text_id", "year"))
final_cy_df <- final_cy_df |> 
    inner_join(select(utable, country_text_id, year),
        by = c("country_text_id", "year"))

# Select columns
final_df <- final_df %>% ungroup |> select(-country_text_id) |>
    rename(v2x_elecreg_calc = v2x_elecreg,
        v2xex_elecreg_calc = v2xex_elecreg,
        v2xlg_elecreg_calc = v2xlg_elecreg) |> 
    arrange(country_id, historical_date)
final_cy_df <- final_cy_df %>% ungroup |> select(-country_text_id) |>
    rename(v2x_elecreg_calc = v2x_elecreg,
        v2xex_elecreg_calc = v2xex_elecreg,
        v2xlg_elecreg_calc = v2xlg_elecreg) |> 
    arrange(country_id, year)

indices <- names(final_df)[!names(final_df) %in% c("country_id", "historical_date", "year")]

out <- list()
invisible(lapply(indices, function(v) {
    out[[v]]$cd <<- final_df[, c("country_id", "historical_date", "year", v)]
    out[[v]]$cy <<- final_cy_df[, c("country_id", "year", v)]
}))

write_file(out, OUTFILE, dir_create = TRUE)
