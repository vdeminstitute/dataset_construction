#!/usr/bin/env Rscript
#
# elecreg.R: creates CD/CY versions of v2x_elecreg & friends
#
# Goodluck understanding the construction rules for
# elecreg. Translated from an old Stata script with no comments. In
# general:
#     1. For each country, create elecreg + constituent indices
#     2. Create cumulative election indices
#     3. Aggregate CY version
###

options(warn = 2)

suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))



db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
OUTFILE <- create_outfile(id = Sys.getenv("TASK_ID"),
                          ROOT = Sys.getenv("ROOT_DIR"),
                          db = db)
ROOTDIR <- ROOT
REFS    <- file.path(ROOTDIR, "refs")
stopifnot(dir.exists(REFS))

varname <- get_varname(Sys.getenv("TASK_ID"), db)
objs <- find_dep_files(Sys.getenv("TASK_ID"), db)

# Load each dependencies and merge
df_load <-
    lapply(names(objs), function(v) objs[[v]][[v]]$cd) %>%
    Reduce(full_join_vdem, .)

df_load %<>% arrange(country_id, historical_date)

qtable <- read_file(file.path(ROOT, "refs/question_table.rds"))
country <- read_file(file.path(ROOT, "refs/country_table.rds"))
utable <- read_file(file.path(ROOT, "refs/country_unit.rds"))

df_load %<>%
    left_join(select(country, country_id, country_text_id),
              by = c("country_id"))

a_vars.df <-
    df_load %>%
    select(country_text_id, year, historical_date, country_id,
           matches("v2eltype"), v2expathhs, v2expathhg, v2exhoshog,
           v2lgbicam, v2lgello) %>%
    arrange(country_text_id, historical_date)

# Interpolate
fill_cols <- c("v2expathhs", "v2expathhg", "v2exhoshog", "v2lgbicam",
               "v2lgello")
a_vars.df %<>%
    left_join(select(utable, country_text_id, year, gap_idx),
              by = c("country_text_id", "year")) %>%
    group_by(country_text_id, year = to_year(historical_date)) %>%
    arrange(historical_date) %>%
    mutate_at(fill_cols, interpolate) %>%
    group_by(country_text_id) %>%
    arrange(country_text_id, desc(historical_date)) %>%
    mutate(gap_idx = locf(gap_idx)) %>%
    arrange(country_text_id, historical_date) %>%
    group_by(country_text_id, gap_idx) %>%
    arrange(country_text_id, historical_date) %>%
    mutate_at(fill_cols, locf) %>%
    ungroup

# Clean after interpolation
a_vars.df %<>%
    mutate(v2lgello = ifelse(v2lgbicam == 0, NA, v2lgello),
           v2expathhg = ifelse(v2exhoshog == 1, NA, v2expathhg))



a.split <- split(a_vars.df,
                 list(a_vars.df$gap_idx, a_vars.df$country_text_id), drop = T)
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
                df$historical_date < "1993-01-01"] <- T
        elechos[df$historical_date > "2000-01-01" &
                df$historical_date < "2005-01-01"] <- T
    }

    # HOG appointed through popular election v2ex_elechog
    elechog <- df$v2expathhg == 8
    # Missing if HOS = HOG
    is.na(elechog) <- df$v2exhoshog != 0

    # Is the HOS no longer elected? v2x_hosinter
    hosinter <- c(NA, diff(elechos) == -1) &
        !(df$v2expathhs == 6 & (df$v2lgbicam %in% c(1, 2)))
    is.na(hosinter) <- is.na(elechos)
    hosinter[is.na(hosinter) & !is.na(elechos)] <- F

    # Is the HOG no longer elected? hoginter
    hoginter <- c(NA, diff(elechog) == -1)
    is.na(hoginter) <- is.na(elechog)
    hoginter[is.na(hoginter) & !is.na(elechog)] <- F
    hoginter[is.na(hoginter)] <- F

    # Percentage of directly elected officials in lower chamber
    df$v2lgello2 <- df$v2lgello
    # If no chamber then set to FALSE (for interpolation)
    df %<>% mutate(v2lgello2 = ifelse(v2lgbicam == 0 & !is.na(v2lgbicam),
                                      0, v2lgello2))
    df$v2lgello2 <- locf(df$v2lgello2)

    # Legislature closed down or aborted v2xlg_leginter
    leginter <- with(df, (v2lgbicam == 0 & lag(v2lgbicam) > 0) |
        (v2lgello2 == 0 & lag(v2lgello2) > 0))
    leginter[is.na(df$v2lgello2)] <- F

    # HOS elections aborted? v2x_hosabort
    hosabort <- elecpres == 1 & elechos == 0 & lead(elechos) == 0 &
        c(F, diff(df$historical_date) < 366)
    is.na(hosabort) <- is.na(elechos)
    hosabort[(elechog == T) | (lead(elechog) == T) |
             (lead(elechog, 2) == T & hosabort == T)] <- F
    # If the HOS is back within 366 days it does not count as aborted.
    for (i in which(hosabort)) {
        if (any(elechos[(i + 1):length(elechos)] &
                df$historical_date[(i + 1):length(df$historical_date)] -
                df$historical_date[i] < 366))
            hosabort[i] <- F
    }
    if (country == "BOL")
        hosabort[between(df$year, 1985, 2002)] <- F
    else if (country == "EST")
        hosabort[df$year == 1992] <- F
    else if (country == "CRI")
        hosabort[df$year == 1913 | df$year == 1919] <- F
    else if (country == "KOR")
        hosabort[df$year == 1960] <- F
    else if (country == "IRN")
        hosabort[between(df$year, 1981, 1985)] <- F

    # Legislative election results aborted? v2x_legabort
    legabort <- with(df, elecparl == 1 & v2lgbicam == 0 & lead(v2lgbicam) == 0 &
        c(F, diff(historical_date) < 366))
    is.na(legabort) <- is.na(elechos)
    # If the legislature is back within 366 days it does not count as aborted.
    for (i in which(legabort)) {
        if (any(df$v2lgbicam[(i + 1):length(df$v2lgbicam)] >= 1 &
                df$historical_date[(i + 1):length(df$historical_date)] -
                df$historical_date[i] < 366))
            legabort[i] <- F
    }

    # Electoral regime v2x_elecreg
    # Election makes an electoral regime
    elecreg <- elecparl | elecpres
    elecreg[!elecreg] <- NA
    # Interruption of election or no longer elected -> FALSE
    elecreg[hosinter | leginter | lag(hosabort) | lag(legabort)] <- F
    elecreg <- locf(elecreg)
    elecreg[is.na(elecreg)] <- F

    # Executive electoral regime index v2xex_elecreg
    ex_elecreg <- elecpres
    ex_elecreg[!ex_elecreg] <- NA
    ex_elecreg[hoginter | hosinter | lag(hosabort)] <- F
    ex_elecreg <- locf(ex_elecreg)
    ex_elecreg[is.na(ex_elecreg)] <- F

    # Legislative electoral regime index v2xlg_elecreg
    lg_elecreg <- elecparl
    lg_elecreg[!lg_elecreg] <- NA
    lg_elecreg[leginter | lag(legabort)] <- F
    lg_elecreg <- locf(lg_elecreg)
    lg_elecreg[is.na(lg_elecreg)] <- F

    data.frame(v2x_elecreg = elecreg,
               v2xex_elecreg = ex_elecreg,
               v2xlg_elecreg = lg_elecreg,
               v2xel_elecparl = elecparl,
               v2xel_elecpres = elecpres,
               v2ex_elechos = elechos,
               v2ex_elechog = elechog,
               v2x_hosinter = hosinter,
               v2xlg_leginter = leginter,
               v2x_hosabort = hosabort,
               v2x_legabort = legabort) %>%
        mutate_all(as.numeric)
})

mc_assert(ll)

rows.df <- bind_rows(ll)
elecreg.df <-
    select(a_vars.df, country_text_id, country_id, historical_date, year, matches("v2eltype")) %>%
    bind_cols(rows.df)

####
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



####
#  Hey, let's aggregate to cy.
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
              by = c("country_id", "country_text_id", "year"))
stopifnot(colnames(cy.df) %in% colnames(final_cy.df))


final.df %<>% ungroup %>% select(-country_text_id) %>%
    rename(v2x_elecreg_calc = v2x_elecreg,
           v2xex_elecreg_calc = v2xex_elecreg,
           v2xlg_elecreg_calc = v2xlg_elecreg)
final_cy.df %<>% ungroup %>% select(-country_text_id) %>%
    rename(v2x_elecreg_calc = v2x_elecreg,
           v2xex_elecreg_calc = v2xex_elecreg,
           v2xlg_elecreg_calc = v2xlg_elecreg)

indices <- names(final.df)[!names(final.df) %in%
                                c("country_id", "historical_date", "year")]

out <- list()
lapply(indices, function(v) {
    out[[v]]$cd <<- final.df[, c("country_id", "historical_date", "year", v)]
    out[[v]]$cy <<- final_cy.df[, c("country_id", "year", v)]
}) %>% invisible

write_file(out, OUTFILE, dir_create = T)
update_task_status(db = db)
