#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# This scripts generates the regimes of the world variable. In current form,
# it uses only country-year data due to how it originally was constructed. Further,
# the output is only post 1900.
# 
# The module that creates regimes of the world is a STATA script (regime_type.do) that is called
# by this R script. Hence, it requires a that STATA is installed and that the
# the symbolic link to STATA is set up correctly (consult the setup script for STATA).
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()

# Load dependencies and reference documents
country <- load_country()
objs <- find_dep_files(TASK_ID, db)
df <- Reduce(
    f = full_join_vdem,
    x = lapply(names(objs), function(v) {
            add_date_cols(
                add_country_cols(
                    objs[[v]][[v]]$cy, country))
    })
)

# Merge in name from country as country_name (needed for the STATA script) 
df <- organiseRows(
    merge(
        x = df,
        y = country[, c("country_id", "name")],
        by = "country_id",
        all.x = TRUE),
    country_id, historical_date, foreground = TRUE)
names(df)[names(df) == "name"] <- "country_name"

# Create extra variables per executive and legislative
vv <- grep("v2elmulpar", names(df), value = TRUE)
info("Creating EX/LG version of " %^% vv)
leg <- paste0(vv, "_leg")
ex <- paste0(vv, "_ex")
df <- bind_cols(df, setNames(df[, vv], leg))
df <- bind_cols(df, setNames(df[, vv], ex))

# Create STATA input file
df <- transform(df,
    v2xel_elecparl = ifelse(is.na(v2xel_elecparl), 0, v2xel_elecparl),
    v2xel_elecpres = ifelse(is.na(v2xel_elecpres), 0, v2xel_elecpres))
is.na(df[, leg]) <- matrix(rep(!df$v2xel_elecparl, length(leg)) , ncol = length(leg))
is.na(df[, ex]) <- matrix(rep(!df$v2xel_elecpres, length(ex)), ncol = length(ex))
df <- df[, grep("95$", names(df), value = TRUE, invert = TRUE)]
colnames(df) <- gsub("68", "", colnames(df))

write_file(
    df,
    file.path(ROOT, "stata/regime_type_input.dta"),
    dir_create = TRUE)

# RUN STATA
# ------------------------------------------------------------------------------
home <- Sys.getenv("HOME")
system("cd " %^% home %^% "/proj/vdemds/stata_indices/ && ~/.local/bin/stata -b do regime_type.do")
# Read and print STATA log output
d <- readLines(home %^% "/proj/vdemds/stata_indices/regime_type.log")
# Remove lines concerning license
d <- d[-(1:21)]
d <- d %>% trimws(which = "both") %>% .[. != ""]
print(shQuote(d))

# Also check log file for error messages and send status error
# STATA will return some error code e.g. r(199) if an error occurs
if (any(grepl("r(", d, fixed = T)) | any(grepl("expired", d))) {
    stop("regime_type failed!")
}

# Read data file and fix
# ------------------------------------------------------------------------------

df <- read_file(file.path(ROOT, "stata/regime_type_output.dta"),
                convert.factors = FALSE)

# Hardcoded values
# -- decided by PIs
df %<>%
    mutate(v2x_regime = case_when(
                country_text_id == "ZWE" & year %in% 1980:1986 ~ 1,
                country_text_id == "BEL" & year %in% 1919:1920 ~ 1,
                country_text_id == "BEL" & year %in% 1921:1924 ~ 3,
                T ~ v2x_regime),
            v2x_regime_amb = case_when(
                country_text_id == "ZWE" & year %in% 1980:1986 ~ 4,
                country_text_id == "BEL" & year %in% 1919:1920 ~ 4,
                country_text_id == "BEL" & year %in% 1921:1924 ~ 9,
                T ~ v2x_regime_amb))

# Prepare outpout
df %<>%
    add_country_cols(country) %>%
    add_date_cols() %>%
    select(country_text_id, country_id, historical_date, year, v2x_regime, v2x_regime_amb) %>% 
    filter(year >= 1900)

out <- list()
out[["v2x_regime"]]$cy <- df
write_file(out, OUTFILE, dir_create = TRUE)
