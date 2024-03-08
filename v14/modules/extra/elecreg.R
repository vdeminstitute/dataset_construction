#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Requires:
# - extra/nonc_cd.rds (country-date version of non-C vars)
#
# In general:
#     1. For each country, create elecreg + constituent indices
#     2. Create cumulative election indices
#     3. Aggregate CY version
# ------------------------------------------------------------------------------
suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()

ROOTDIR <- ROOT
REFS    <- file.path(ROOTDIR, "refs")
TASK_ID <- Sys.getenv("VPIPE_TASK_ID")
stopifnot(dir.exists(REFS))
varname <- get_task_name(TASK_ID, db)
objs <- find_dep_files(TASK_ID, db)
qtable <- load_qtable()
country <- load_country()
utable <- load_country_unit()

# ------------------------------------------------------------------------------
# Reduce all dependencies to one data.frame, add country_text_id
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
a_vars.df <-
    organiseRows(df_load[, a_vars_keep], country_text_id, historical_date) |> 
    merge(x = _, y = utable[, c("country_text_id", "year", "gap_idx")],
        by = c("country_text_id", "year"), all.x = TRUE) 

# Interpolate
fill_cols <- c("v2expathhs", "v2expathhg", "v2exhoshog", "v2lgbicam", "v2lgello")
splits <- split(a_vars.df, list(a_vars.df$country_text_id, a_vars.df$year), drop = TRUE)
a_vars.df <-
    # interpolate missing values within CTI and YEAR
    parallel::mclapply(splits, function(dat, FC) {
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
    # interpolate missing values within CTI and GAP_IDX
    (\(x) split(x = x, f = list(x$country_text_id, x$gap_idx), drop = TRUE))() |>
    parallel::mclapply(X = _, function(dat, FC) {
        dat <- organiseRows(dat, historical_date)
        for (fc in FC) { dat[[fc]] <- locf(dat[[fc]]) }
        return(dat)
    }, FC = fill_cols, mc.cores = 12) |>
    do.call(rbind.data.frame, args = _) |>
    organiseRows(country_text_id, historical_date) 

# Clean after interpolation
a_vars.df <- within(a_vars.df, {
    v2lgello = ifelse(v2lgbicam == 0, NA, v2lgello)
    v2expathhg = ifelse(v2exhoshog == 1, NA, v2expathhg)
})

a.split <- split(
    x=a_vars.df,
    f = list(a_vars.df$gap_idx, a_vars.df$country_text_id),
    drop = TRUE)

ll <- lapply(a.split, function(df) {
    country <- unique(df$country_text_id)
    # Legislative or constituent assembly election v2xel_elecparl
    elecparl <- with(df, v2eltype_0 == 1 | v2eltype_1 == 1 |
                         v2eltype_4 == 1 | v2eltype_5 == 1)
    elecparl[is.na(elecparl)] <- F

    # Presidential election v2xel_elecpres                         
    elecpres <- with(df, v2eltype_6 == 1 | v2eltype_7 == 1)
    elecpres[is.na(elecpres)] <- F
    
    # Legislative or presidential election v2ex_elechog
    elec <- elecpres | elecparl

    # HOS appointed through popular election v2ex_elechos
    # Leave as missing if v2expathhg is missing
    elechos  <- df$v2expathhs == 7
    if (country == "PHL") {
        elechos[df$historical_date > "1985-01-01" &
                df$historical_date < "1993-01-01"] <- TRUE
        elechos[df$historical_date > "2000-01-01" &
                df$historical_date < "2005-01-01"] <- TRUE
    }

    # HOG appointed through popular election v2ex_elechog
    elechog <- df$v2expathhg == 8
    # Missing if HOS = HOG
    is.na(elechog) <- df$v2exhoshog != 0

    # Is the HOS no longer elected? v2x_hosinter
    hosinter <- c(NA, diff(elechos) == -1) &
        !(df$v2expathhs == 6 & (df$v2lgbicam %in% c(1, 2)))
    is.na(hosinter) <- is.na(elechos)
    hosinter[is.na(hosinter) & !is.na(elechos)] <- FALSE

    # Is the HOG no longer elected? hoginter
    hoginter <- c(NA, diff(elechog) == -1)
    is.na(hoginter) <- is.na(elechog)
    hoginter[is.na(hoginter) & !is.na(elechog)] <- FALSE
    hoginter[is.na(hoginter)] <- FALSE

    # Percentage of directly elected officials in lower chamber
    df$v2lgello2 <- df$v2lgello
    # If no chamber then set to FALSE (for interpolation)
    df %<>% mutate(v2lgello2 = ifelse(v2lgbicam == 0 & !is.na(v2lgbicam),
                                      0, v2lgello2))
    df$v2lgello2 <- locf(df$v2lgello2)

    # Legislature closed down or aborted v2xlg_leginter
    leginter <- with(df, (v2lgbicam == 0 & lag(v2lgbicam) > 0) |
        (v2lgello2 == 0 & lag(v2lgello2) > 0))
    leginter[is.na(df$v2lgello2)] <- FALSE

    # HOS elections aborted? v2x_hosabort
    hosabort <- elecpres == 1 & elechos == 0 & lead(elechos) == 0 &
        c(F, diff(df$historical_date) < 366)
    is.na(hosabort) <- is.na(elechos)
    hosabort[(elechog == TRUE) | (lead(elechog) == TRUE) |
             (lead(elechog, 2) == TRUE & hosabort == TRUE)] <- FALSE
    # If the HOS is back within 366 days it does not count as aborted.
    for (i in which(hosabort)) {
        if (any(elechos[(i + 1):length(elechos)] &
                df$historical_date[(i + 1):length(df$historical_date)] -
                df$historical_date[i] < 366))
            hosabort[i] <- FALSE
    }
    if (country == "BOL")
        hosabort[between(df$year, 1985, 2002)] <- FALSE
    else if (country == "EST")
        hosabort[df$year == 1992] <- FALSE
    else if (country == "CRI")
        hosabort[df$year == 1913 | df$year == 1919] <- FALSE
    else if (country == "KOR")
        hosabort[df$year == 1960] <- FALSE
    else if (country == "IRN")
        hosabort[between(df$year, 1981, 1985)] <- FALSE

    # Legislative election results aborted? v2x_legabort
    legabort <- with(df, elecparl == 1 & v2lgbicam == 0 & lead(v2lgbicam) == 0 &
        c(FALSE, diff(historical_date) < 366))
    is.na(legabort) <- is.na(elechos)
    # If the legislature is back within 366 days it does not count as aborted.
    for (i in which(legabort)) {
        if (any(df$v2lgbicam[(i + 1):length(df$v2lgbicam)] >= 1 &
                df$historical_date[(i + 1):length(df$historical_date)] -
                df$historical_date[i] < 366))
            legabort[i] <- FALSE
    }

    # Electoral regime v2x_elecreg
    # Election makes an electoral regime
    elecreg <- elecparl | elecpres
    elecreg[!elecreg] <- NA
    # Interruption of election or no longer elected -> FALSE
    elecreg[hosinter | leginter | lag(hosabort) | lag(legabort)] <- FALSE
    elecreg <- locf(elecreg)
    elecreg[is.na(elecreg)] <- FALSE

    # Executive electoral regime index v2xex_elecreg
    ex_elecreg <- elecpres
    ex_elecreg[!ex_elecreg] <- NA
    ex_elecreg[hoginter | hosinter | lag(hosabort)] <- FALSE
    ex_elecreg <- locf(ex_elecreg)
    ex_elecreg[is.na(ex_elecreg)] <- FALSE

    # Legislative electoral regime index v2xlg_elecreg
    lg_elecreg <- elecparl
    lg_elecreg[!lg_elecreg] <- NA
    lg_elecreg[leginter | lag(legabort)] <- FALSE
    lg_elecreg <- locf(lg_elecreg)
    lg_elecreg[is.na(lg_elecreg)] <- FALSE

    out <- data.frame(
        v2x_elecreg = elecreg,
        v2xex_elecreg = ex_elecreg,
        v2xlg_elecreg = lg_elecreg,
        v2xel_elecparl = elecparl,
        v2xel_elecpres = elecpres,
        v2ex_elechos = elechos,
        v2ex_elechog = elechog,
        v2x_hosinter = hosinter,
        v2xlg_leginter = leginter,
        v2x_hosabort = hosabort,
        v2x_legabort = legabort)
    out[] <- lapply(out, as.numeric)
    
    return(out)
})

mc_assert(ll)

elecreg.df <-
    select(
        a_vars.df, country_text_id, country_id, historical_date, year,
        matches("v2eltype")) |> 
    bind_cols(bind_rows(ll)) |> 
    organiseRows(country_id, historical_date)

# Override elecreg for countries with left-censoring issue (ie
# codingstart 1900 w/ no hist data, but first elections were in 19th
# century) and Libya & Syria since war.
elecreg.df %<>% mutate(v2x_elecreg = case_when(country_text_id == "ZWE" & year %in% c(1900, 1901, 1902) ~ 1,
                                            country_text_id == "JAM" & year == 1900 ~ 1,
                                            country_text_id == "ISL" & year == 1900 ~ 1,
                                            country_text_id == "SYR" & year >= 2013 ~ 0,
                                            country_text_id == "LBY" & year >= 2014 ~ 0,
                                            T ~ v2x_elecreg),
                    v2xex_elecreg = case_when(country_text_id == "SYR" & year >= 2013 ~ 0,
                                              T ~ v2xex_elecreg),
                    v2xlg_elecreg = case_when(country_text_id == "ZWE" & year %in% c(1900, 1901, 1902) ~ 1,
                                              country_text_id == "JAM" & year == 1900 ~ 1,
                                              country_text_id == "ISL" & year == 1900 ~ 1,
                                              country_text_id == "SYR" & year >= 2013 ~ 0,
                                              country_text_id == "LBY" & year >= 2014 ~ 0,
                                              T ~ v2xlg_elecreg))

###
# Let's construct the cumulative election indices. As listed in the
# codebook, this is from 1900 until present.
elecons.df <- filter(elecreg.df, historical_date >= '1900-01-01') %>%
    group_by(country_text_id) %>%
    arrange(historical_date) %>%
    mutate(v2ellocumul = cumsum((!is.na(v2eltype_0) & v2eltype_0 == 1) |
                                (!is.na(v2eltype_1) & v2eltype_1 == 1)),
           v2elprescumul = cumsum((!is.na(v2eltype_6) & v2eltype_6 == 1) |
                                  (!is.na(v2eltype_7) & v2eltype_7 == 1)))

# Consecutive lower chamber elections
elecons.df %<>%
    group_by(country_text_id) %>%
    arrange(historical_date) %>%
    mutate(regime = (c(F, diff(v2xlg_elecreg) != 0) %>% cumsum)) %>%
    group_by(country_text_id, regime) %>%
    mutate(v2ellocons = cumsum((!is.na(v2eltype_0) & v2eltype_0 == 1) |
                               (!is.na(v2eltype_1) & v2eltype_1 == 1))) %>%
    ungroup %>% select(-regime)

# Consecutive presidential elections
elecons.df %<>%
    group_by(country_text_id) %>%
    arrange(historical_date) %>%
    mutate(regime = (c(F, diff(v2xex_elecreg) != 0) %>% cumsum)) %>%
    group_by(country_text_id, regime) %>%
    mutate(v2elprescons = cumsum((!is.na(v2eltype_6) & v2eltype_6 == 1) |
                                 (!is.na(v2eltype_7) & v2eltype_7 == 1))) %>%
    ungroup %>% select(-regime)

final.df <-
    elecons.df %>%
    select(country_text_id, historical_date, v2ellocumul,
           v2elprescumul, v2ellocons, v2elprescons) %>%
    right_join(elecreg.df, by = c("country_text_id", "historical_date")) %>%
    select(-matches("v2eltype"))

# Aggregate to country-year format 
cy.df <- select(final.df, country_text_id, country_id, year,
               v2x_elecreg, v2xex_elecreg, v2xlg_elecreg,
               v2xel_elecparl, v2xel_elecpres, v2ex_elechos,
               v2ex_elechog, v2x_hosinter,
               v2ellocumul, v2elprescumul, v2ellocons,
               v2elprescons, v2x_legabort, v2x_hosabort,
               v2xlg_leginter)

# This are cumulative counts, so aggregate by last
last_vars <- c("v2ellocumul", "v2elprescumul", "v2ellocons", "v2elprescons")
last.df <-
    cy.df %>%
    select(country_id, country_text_id, year, one_of(last_vars)) %>%
    group_by(country_id, country_text_id, year) %>%
    summarise_all(collect_last)

# The rest are event-specific, so aggregate by max
max.df <-
    cy.df %>%
    select(-one_of(last_vars)) %>%
    group_by(country_id, country_text_id, year) %>%
    summarise_all(collect_max)

final_cy.df <-
    full_join(last.df, max.df,
              by = c("country_id", "country_text_id", "year")) %>%
	ungroup
stopifnot(colnames(cy.df) %in% colnames(final_cy.df))

# Clean by utable
final.df %<>% inner_join(select(utable, country_text_id, year),
	by = c("country_text_id", "year"))
final_cy.df %<>% inner_join(select(utable, country_text_id, year),
	by = c("country_text_id", "year"))

# Select columns
final.df %<>% ungroup %>% select(-country_text_id) %>%
    rename(v2x_elecreg_calc = v2x_elecreg,
           v2xex_elecreg_calc = v2xex_elecreg,
           v2xlg_elecreg_calc = v2xlg_elecreg) %>% 
    arrange(country_id, historical_date)
final_cy.df %<>% ungroup %>% select(-country_text_id) %>%
    rename(v2x_elecreg_calc = v2x_elecreg,
           v2xex_elecreg_calc = v2xex_elecreg,
           v2xlg_elecreg_calc = v2xlg_elecreg) %>% 
    arrange(country_id, year)

indices <- names(final.df)[!names(final.df) %in% c("country_id", "historical_date", "year")]

out <- list()
invisible(lapply(indices, function(v) {
    out[[v]]$cd <<- final.df[, c("country_id", "historical_date", "year", v)]
    out[[v]]$cy <<- final_cy.df[, c("country_id", "year", v)]
}))

write_file(out, OUTFILE, dir_create = TRUE)
