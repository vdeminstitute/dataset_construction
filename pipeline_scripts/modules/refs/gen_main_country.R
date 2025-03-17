#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Purpose:
# Generate a data.frame that contains the main country coded for each coder.
# ------------------------------------------------------------------------------

options(warn = 2)
suppressMessages(library(dplyr))
suppressMessages(library(DBI))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# -- Get database for pipeline
db <- vbase::pg_connect(Sys.getenv("PIPE_DB"))
# -- Global variables
get_globals()
OUTDIR <- file.path(ROOT, "refs")
INDIR <- file.path(ROOT, "download")
OUTFILE <- file.path(OUTDIR, "main_country.rds")

# -- Read tables
country <- read_file(file.path(ROOT, "refs", "country_table.rds"))
coder <-  read_file(file.path(ROOT, "download", "coder.rds"))

# -- Remove regions
country %<>% filter(!grepl("*", name, fixed = TRUE))

# -- In coder, recode country to country_name and remove test coders
coder <-
    coder %>%
    select(coder_id, country_name = country, username) %>%
    mutate(country_name = ifelse(country_name == "",
        yes = NA_character_,
        no = country_name)) %>%
    filter(!grepl("[tT]est", username))

# -- Match country_name to the country table.
coder %<>% mutate(country_name = case_when(
    country_name == "CHINA." ~ "China",
    country_name == "Congo, Democratic Republic of the" ~ "Democratic Republic of the Congo",
    country_name == "Congo, Democratic Republic of" ~ "Democratic Republic of the Congo",
    country_name == "Congo, Republic of the" ~ "Republic of the Congo",
    country_name == "East Timor" ~ "Timor-Leste",
    country_name == "Egypt, Ottoman Empire" ~ "Egypt",
    country_name == "Gambia" ~ "The Gambia",
    country_name == "New ZeaLand" ~ "New Zealand",
    country_name == "Palestine (British Mandate)" ~ "Palestine/British Mandate",
    country_name == "Palestine (Gaza)" ~ "Palestine/Gaza",
    country_name == "Palestine (West Bank)" ~ "Palestine/West Bank",
    country_name == "Sao Tomé and Príncipe" ~ "Sao Tome and Principe",
    country_name == "Sardinia-Piedmont" ~ "Piedmont-Sardinia",
    country_name == "Swaziland" ~ "Eswatini",
    country_name == "sweden" ~ "Sweden",
    country_name == "Timor Leste" ~ "Timor-Leste",
    country_name == "United States" ~ "United States of America",
    country_name == "Vietnam, Democratic Republic of" ~ "Vietnam",
    country_name == "Vietnam, Republic of" ~ "Republic of Vietnam",
    country_name == "Yemen, South" ~ "South Yemen",
    country_name == "Macedonia" ~ "North Macedonia",
    country_name == "nigeria" ~ "Nigeria",
	country_name == "Côte d'Ivoire" ~ "Ivory Coast",
    country_name == "Czech Republic" ~ "Czechia",
    country_name == "Turkey" ~ "Türkiye",
    TRUE ~ country_name))

# Throw an error if there is a country that was not matched correctly
bad_countries <- unique(sort(coder$country_name[!coder$country_name %in% country$name]))
if (length(bad_countries) != 0) {
    info("Bad main country for coders, check the coder table for faulty name: " %^% bad_countries)
    stop("error")
}

# The remaining coders that are missing a country do not have any ratings.
res <- coder[!is.na(coder$country_name), ]
stopifnot(`country_name should not have NAs` = !anyNA(res$country_name))
info(sprintf("Found %d coders with a main country", nrow(res)))
info(sprintf("Removed %d coders that are missing a main country",
    nrow(coder) - nrow(res)))

# Merge country_id and country_text_id
out <- merge(
    x = res,
    y = country[, c("country_id", "name", "country_text_id")],
    by.x = "country_name",
    by.y = "name",
    all.x = TRUE)
write_file(out, OUTFILE, dir_create = TRUE)