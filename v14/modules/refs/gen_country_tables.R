#!/usr/bin/env Rscript

# Purpose: 
# This script creates the reference files:
# 1: country_unit
# 2: country_table
# 3: psq_country_table
# These references are used to determine the structure and characteristics of countries.

suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db_vdem <- pg_connect(Sys.getenv("DOWNLOAD_DB"))
ROOT <- Sys.getenv("ROOT_DIR")
INDIR <- file.path(ROOT,"download")
OUTDIR <- file.path(ROOT, "refs")
dir.create(OUTDIR, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# Functions

create_psq_translation_table <- function(dir = INDIR, drop_ids) {

    ptable <-
        read_file(file.path(dir, "country_list.rds")) %>%
        filter(!country_list_id %in% drop_ids) %>% 
        mutate(psq_country_id = country_list_id, name = trimws(name)) %>% 
        select(-country_list_id)
    
    return(ptable)
}

create_country_unit_table <- function(dir = INDIR) {

    # country-year mask: what cy's are codeable?
    cy_mask <- read_file(file.path(dir, "country_year_mask.rds"))
    # country: which countries exists?
    country <- read_file(file.path(dir, "country.rds")) %>% 
        rename(country_text_id = text_id)
    # succesion id
    succession <-
        read_file(file.path(dir, "country_succession.rds")) %>%
        select(country_id = child_country_id, end_date)
    
    country_unit <-
        cy_mask %>%
        # Only contemporary-only/historical-only
        filter(mask_id %in% c(3, 8)) %>%
        # Exclude Big China
        filter(country_id != 213) %>% 
        # Turn Nejd into Saudi Arabia
        mutate(country_id = ifelse(country_id == 374, 197, country_id)) %>%
        # Recode everything that is not contemporary-only to historical-only
        mutate(mask_id = ifelse(mask_id == 3, "contemporary-only", "historical-only")) %>%
        mutate(country_text_id = to_ctext_ids(country_id, country)) %>%
        # Define the group 'overlap' from data
        group_by(country_text_id, country_id, year) %>%
        summarise(project = ifelse(n() == 1, mask_id, "overlap")) %>%
        left_join(succession, by = "country_id") %>%
        filter(is.na(end_date) | year <= end_date) %>%
        mutate(historical_date = as.Date(year %^% "-12-31"), end_date = NULL) %>%
        mutate(year = as.numeric(year)) %>% 
        ungroup() %>%
        arrange(country_id, year) %>% 
        mutate(., cu_id = seq_along(rownames(.))) %>% # cu_id = row id for postgres
        select(cu_id, country_id, year, project)
    stopifnot(!dplyr::is_grouped_df(country_unit))

    return(country_unit)

}

create_utable <- function(cu_table, c_table) {

    stopifnot(all(unique(cu_table$country_id) %in% c_table$country_id))

    utable <-
        cu_table %>%
        arrange(country_id, year) %>%
        left_join(c_table[, c("country_id", "country_text_id")],
            by = "country_id") %>%
        mutate(historical_date = year %^% "-12-31")  %>%
        group_by(country_id) %>%
        arrange(year) %>%
        mutate(gap_idxs = create_idx(year)) %>%
        group_by(country_id, gap_idxs) %>%
        mutate(gap_idx = dplyr::cur_group_id()) %>%
        ungroup() %>%
        arrange(country_id, year) %>%
        select(-gap_idxs)

    return(utable)
}

create_country_table <- function(u_table, country) {

    country_table <-
        u_table %>%
        select(-gap_idx) %>%
        group_by(country_text_id) %>%
        summarise(
            codingstart_contemp =
                min(year[project %in% c("contemporary-only", "overlap")] %||% NA),
            codingend_contemp =
                max(year[project %in% c("contemporary-only", "overlap")] %||% NA),
            codingstart_hist =
                min(year[project %in% c("historical-only", "overlap")] %||% NA),
            codingend_hist =
                max(year[project %in% c("historical-only", "overlap")] %||% NA)) %>%
        ungroup() %>% 
        right_join(country, by = "country_text_id")

    return(country_table)
}


# ------------------------------------------------------------------------------
# These tables are used as means to create and filter the reference documents
# that are produced in this script.
country_succession <-
    read_file(file.path(INDIR, "country_succession.rds")) %>%
    select(child_country_id, parent_country_id, end_date)

country <-
    read_file(file.path(INDIR, "country.rds")) %>%
    select(country_text_id = text_id, country_id, name) %>%
    filter(country_id != 213) %>%
    left_join(country_succession, by = c("country_id" = "child_country_id")) %>%
    mutate(name = trimws(name)) %>% 
    arrange(country_id)

# ------------------------------------------------------------------------------
# ptable = table that shows the name and country_id from the PSQ questionnaire.

ptable <- create_psq_translation_table(dir = INDIR, drop_ids = 197)
write_file(ptable, file.path(OUTDIR, "psq_country_table.rds"))

# ------------------------------------------------------------------------------
# country_unit = table that tracks countries and their potentials gaps over time.
# We only use mask_id 3 (contemporary-only) and 8 (historical-only) from
# country_year_mask to define our units.

country_unit <- create_country_unit_table(dir = INDIR)
# TRUNCATE TABLE: we remove all rows from the current table in postgres
pg_send_query(db_vdem, "TRUNCATE TABLE country_unit;")
# UPLOAD NEW TABLE
pg_append_table(country_unit, "country_unit", db_vdem)

# ------------------------------------------------------------------------------
# utable = unit table per country over time and their consecutive periods. 
utable <- create_utable(cu_table = country_unit, c_table = country)
write_file(utable, file.path(OUTDIR, "country_unit.rds"))

# ------------------------------------------------------------------------------
# country_table = cross-section unit table 
country_table <- create_country_table(u_table = utable, country = country)
write_file(country_table, file.path(OUTDIR, "country_table.rds"))

info("Done creating country tables")