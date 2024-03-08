#!/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
OUTFILE <- create_outfile(id = Sys.getenv("TASK_ID"),
                          ROOT = Sys.getenv("ROOT_DIR"),
                          db = db)
country <- read_file(file.path(ROOT, "refs", "country_table.rds"))
# Load dependencies
objs <- find_dep_files(Sys.getenv("TASK_ID"), db)

# Load each dependency and merge
df <-
    lapply(names(objs), function(v) {
            objs[[v]][[v]]$cy %>% add_country_cols(country) %>%
            add_date_cols
    }) %>%
    Reduce(full_join_vdem, .)

df %<>% arrange(country_id, historical_date)
df %<>% left_join(select(country, country_id, country_name = name),
                  by = "country_id")

# Create extra variables (ex leg)
vv <- grep("v2elmulpar", names(df), value = TRUE)
info("Creating EX/LG version of " %^% vv)
leg <- vv %^% "_leg"
ex <- vv %^% "_ex"
df <- bind_cols(df, df[, vv] %>% setNames(leg))
df <- bind_cols(df, df[, vv] %>% setNames(ex))

df %<>%
    mutate(v2xel_elecparl = ifelse(is.na(v2xel_elecparl), 0, v2xel_elecparl),
           v2xel_elecpres = ifelse(is.na(v2xel_elecpres), 0, v2xel_elecpres))
is.na(df[, leg]) <-
    rep(!df$v2xel_elecparl, length(leg)) %>%
    matrix(ncol = length(leg))
is.na(df[, ex]) <-
    rep(!df$v2xel_elecpres, length(ex)) %>%
    matrix(ncol = length(ex))

df %<>% select(-matches("95"))
colnames(df) <- gsub("68", "", colnames(df))
write_file(df, file.path(ROOT, "stata/regime_type_input.dta"),
           dir_create = T)

# Run STATA script
##----------------------------------------------------------------
home <- Sys.getenv("HOME")
system("cd " %^% home %^% "/proj/mm-prep/do/ && ~/stata/stata-se -b do regime_type.do")
# Read and print STATA log output
d <- readLines(home %^% "/proj/mm-prep/do/regime_type.log")
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
##----------------------------------------------------------------

df <- read_file(file.path(ROOT, "stata/regime_type_output.dta"),
                convert.factors = F)

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


df %<>%
    add_country_cols(country) %>%
    add_date_cols %>%
    select(country_text_id, country_id, historical_date, year, v2x_regime,
        v2x_regime_amb)

# Remove before 1900
df %<>% filter(year >= 1900)

out <- list()
out[["v2x_regime"]]$cy <- df
write_file(out, OUTFILE, dir_create = T)
update_task_status(db = db)
