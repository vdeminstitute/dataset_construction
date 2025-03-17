#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# This script takes the e data stored in the e_data table and creates a static file in the correct ds_construction folder.
# This script also interpolates the relevant variables and creates e_miurbani.
# ------------------------------------------------------------------------------

suppressMessages(library(vutils))
suppressMessages(library(dplyr))
suppressMessages(library(vanalysis))
suppressMessages(library(DBI))

current_year <- Sys.getenv("NEWEST_YEAR")
EXT_DS_DIR <- sprintf("~/data/data_team/external_data/external_data_%s", current_year)
tt <- read_file(list.files(file.path(EXT_DS_DIR, "identifiers"), pattern = "tt", full.names = TRUE)) %>%
    mutate(country_name = case_when(country_id == 99 ~ "Türkiye", TRUE ~ country_name))

OUTFILE <- file.path(Sys.getenv("ROOT_DIR"), "external")
if (!dir.exists(OUTFILE)) dir.create(OUTFILE)

country_unit <- load_country_unit()
vdemdata <- pg_connect("vdem_data")

e_data <- dbGetQuery(vdemdata, "SELECT tag, country_id, year, code FROM e_data;") %>%
    long_to_wide(id_vars = c("country_id", "year"), id_var = "tag", value_var = "code")    

stopifnot(nrow(e_data) == nrow(tt))

# ------------------------------------------------------------------------------
# Functions
check_years <- function(df) {
    check_years <- df %>% 
        select(country_id, year, starts_with("e_")) %>%
        wide_to_long(., id_vars = c("country_id", "year")) %>%
        filter(!is.na(value)) %>%
        arrange(variable, year) %>%
        group_by(variable) %>%
        mutate(min_year = min(year),
            max_year = max(year)) %>%
        ungroup() %>%
        distinct(variable, min_year, max_year)

    return(check_years)
}

# ------------------------------------------------------------------------------
# Missing data within a time–series is interpolated using linear interpolation for each country. In addition to this, from the last recorded data point to nowadays the data is extrapolated: e_peaveduc 

# Missing data within a time–series is interpolated using linear interpolation: e_mipopula, e_miurbpop, e_pefeliex, e_pechmor, e_pelifeex, e_miinflat

linint_vars <- c("e_mipopula", "e_miurbpop", "e_pefeliex", "e_pechmor", "e_pelifeex", "e_miinflat", "e_peaveduc")
stopifnot(linint_vars %in% names(e_data))

e_int <- e_data %>% 
	group_by(country_id) %>%
	arrange(year) %>%
	mutate_at(vars(all_of(linint_vars)), function(col) {
		zoo::na.approx(col, method = "linear", na.rm = FALSE, rule = 1)
        }) %>%
	ungroup() %>%
	mutate(e_miurbani = round(e_miurbpop / e_mipopula, 5)) %>%
	arrange(country_id, year)

# Check interpolation
check_before <- select(e_data, country_id, year, all_of(linint_vars))
check_after <- select(e_int, country_id, year, all_of(linint_vars)) 

combined <- full_join(check_before, check_after, by = c("country_id", "year"), suffix = c("_before", "_after")) %>%
    mutate(diff = case_when(
        (is.na(e_mipopula_before) & !is.na(e_mipopula_after)) |
            (!is.na(e_mipopula_before) & is.na(e_mipopula_after)) | (e_mipopula_before != e_mipopula_after) ~ "e_mipopula",
        (is.na(e_miurbpop_before) & !is.na(e_miurbpop_after)) | 
            (!is.na(e_miurbpop_before) & is.na(e_miurbpop_after)) | (e_miurbpop_before != e_miurbpop_after) ~ "e_miurbpop",
        (is.na(e_pefeliex_before) & !is.na(e_pefeliex_after)) | 
            (!is.na(e_pefeliex_before) & is.na(e_pefeliex_after)) | (e_pefeliex_before != e_pefeliex_after) ~ "e_pefeliex",
        (is.na(e_pechmor_before) & !is.na(e_pechmor_after)) | 
            (!is.na(e_pechmor_before) & is.na(e_pechmor_after)) | (e_pechmor_before != e_pechmor_after) ~ "e_pechmor",
        (is.na(e_pelifeex_before) & !is.na(e_pelifeex_after)) | 
            (!is.na(e_pelifeex_before) & is.na(e_pelifeex_after)) | (e_pelifeex_before != e_pelifeex_after) ~ "e_pelifeex",
        (is.na(e_miinflat_before) & !is.na(e_miinflat_after)) | 
            (!is.na(e_miinflat_before) & is.na(e_miinflat_after)) | (e_miinflat_before != e_miinflat_after) ~ "e_miinflat",
        (is.na(e_peaveduc_before) & !is.na(e_peaveduc_after)) | 
            (!is.na(e_peaveduc_before) & is.na(e_peaveduc_after)) | (e_peaveduc_before != e_peaveduc_after) ~ "e_peaveduc",
        TRUE ~ NA_character_)) %>%
    filter(!is.na(diff))

e_data_years <- check_years(e_data) %>% filter(variable %in% linint_vars)
after_years <- check_years(e_int) %>% filter(variable %in% linint_vars)

stopifnot(all(e_data_years$min_year == after_years$min_year))
stopifnot(all(e_data_years$max_year == after_years$max_year))

# Extrapolate e_peaveduc
e_ext <- e_int %>%
    group_by(country_id) %>%
    arrange(year) %>%
    mutate(e_peaveduc = case_when(
        year >= 2010 & is.na(e_peaveduc) ~ locf(e_peaveduc),
        TRUE ~ e_peaveduc)) 

# Check extrapolation
check_before <- select(e_int, country_id, year, e_peaveduc)
check_after <- select(e_ext, country_id, year, e_peaveduc)

combined <- full_join(check_before, check_after, by = c("country_id", "year"), suffix = c("_before", "_after")) %>%
    mutate(diff = case_when(
        (is.na(e_peaveduc_before) & !is.na(e_peaveduc_after)) |
            (!is.na(e_peaveduc_before) & is.na(e_peaveduc_after)) | (e_peaveduc_before != e_peaveduc_after) ~ "e_peaveduc",
        TRUE ~ NA_character_)) %>%
    filter(!is.na(diff))

# ensure that we have only the cases from the country-unit table
check <- e_ext %>% 
    semi_join(country_unit, by = c("country_id", "year"))
stopifnot(nrow(check) == nrow(unique(check[,c("country_id", "year")])))

check <- e_ext %>% 
    merge(y = tt, by = c("year", "country_id"), all.x = TRUE)
stopifnot(nrow(check) == nrow(unique(check[,c("country_id", "year")])))

# Write to file
write_file(e_int, file.path(OUTFILE,"e_data.rds")) 