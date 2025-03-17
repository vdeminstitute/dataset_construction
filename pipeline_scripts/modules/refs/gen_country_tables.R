#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Purpose: 
# This script creates the reference files:
# 1: country_unit
# 2: country_table
# 3: psq_country_table
# These references are used to determine the structure and characteristics of countries.
# ------------------------------------------------------------------------------

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
    cy_mask <- read_file(file.path(dir, "country_year_mask.rds"))
    country <- read_file(file.path(dir, "country.rds")) %>% 
        rename(country_text_id = text_id)
    succession <-
        read_file(file.path(dir, "country_succession.rds")) %>%
        select(country_id = child_country_id, end_date)
    
    country_unit <-
        cy_mask %>%
        filter(mask_id %in% c(3, 8)) %>%
        filter(country_id != 213) %>% 
        mutate(country_id = ifelse(country_id == 374, 197, country_id)) %>%
        mutate(mask_id = ifelse(mask_id == 3, "contemporary-only", "historical-only")) %>%
        mutate(country_text_id = to_ctext_ids(country_id, country)) %>%
        group_by(country_text_id, country_id, year) %>%
        summarise(project = ifelse(n() == 1, mask_id, "overlap")) %>%
        left_join(succession, by = "country_id") %>%
        filter(is.na(end_date) | year <= end_date) %>%
        mutate(historical_date = as.Date(year %^% "-12-31"), end_date = NULL) %>%
        mutate(year = as.numeric(year)) %>% 
        ungroup() %>%
        arrange(country_id, year) %>% 
        mutate(., cu_id = seq_along(rownames(.))) %>% 
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

# Parent-child table: Ex. Germany (77) is parent to its child Baden (349)
# child_country_id identifies the child while parent_country_id identifies the parent.
# The end_date column defines the year of integration. The relationship 
# Germany--Baden has an end_date of 1871, which means that they are merged into
# one entity as of 1872.
country_succession <-
    read_file(file.path(INDIR, "country_succession.rds")) %>%
    select(child_country_id, parent_country_id, end_date)

# Country table: Cross-sectional country structure without the time component.
# We left join with country_succession to obtain a potential parent and end_date.
# Hence, country table filtered for Baden will have parent_country_id == 77 and
# end_date == 1871, as this is its relationship to Germany.
# We drop country_id = 213 because we used to have two ID's for China. 
# This is not the case any more. Hence, we only keep the other country_id (110).
country <-
    read_file(file.path(INDIR, "country.rds")) %>%
    select(country_text_id = text_id, country_id, name) %>%
    filter(country_id != 213) %>%
    left_join(country_succession, by = c("country_id" = "child_country_id")) %>%
    mutate(name = trimws(name)) %>% 
    arrange(country_id)

# ------------------------------------------------------------------------------
# ptable = table that shows the name and country_idfrom the PSQ questionnaire.
# When experts choose their country in the PSQ, they can choose between the 
# countries that exists in this table. The country_list_id == 197 is removed
# because it is the selection for "Please Select A Country".

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
# This table builds from country_unit and adds extra information.
# The column gap_idx is defined to identify periods within countries that are
# consecutive in years. Thus, if a country has a break (e.g Germany post-WW2),
# the gap_idx will show this.

utable <- create_utable(cu_table = country_unit, c_table = country)
write_file(utable, file.path(OUTDIR, "country_unit.rds"))

# ------------------------------------------------------------------------------
# country_table = cross-section unit table 
# This table contains information about when, for each country, the coding periods
# starts and stop. For example, the columns codingstart and codingend suffixed 
# with either _contemp or _hist shows the start and end year of the corresponding
# periods. Do note that this is not sensitive towards any gaps. For that, we use
# utable. It also has inoformation about parent_country_id and end_date.

country_table <- create_country_table(u_table = utable, country = country)
write_file(country_table, file.path(OUTDIR, "country_table.rds"))

info("Done creating country tables")