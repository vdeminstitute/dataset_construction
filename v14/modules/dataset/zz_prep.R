#!/usr/bin/env Rscript
# ==========================================================================
# We are loading the Post survey questionnaire (PSQ) data file 
# which contains all PSQ data that V-Dem has.
# Some variables get extracted and prepared for adding to the
# coder-level dataset.
# Some of these variables get transformed in order to make it more difficult
# to identify the experts.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vanalysis))
suppressMessages(library(vpipe))

FILEPATH <- "~/data/backups/comments_psq/psq.rds"

# Variables that we want to extract from the PSQ
vars <- c(
    "v2zztimespent", "v2zzmaterials",
    "v2zzfirstreas", "v2zzsecreas", "v2zzsatisf",
    "v2zzfremrk", "v2zzelcdem", "v2zzlibdem", "v2zzmajdem",
    "v2zzcondem", "v2zzprtdem", "v2zzdeldem", "v2zzegldem",
    "v2zzdemcr", "v2zzdemcu", "v2zzdemin", "v2zzdemni", "v2zzdemnk",
    "v2zzdemru", "v2zzdemsar", "v2zzdemsaf", "v2zzdemswe", "v2zzdemswz",
    "v2zzdemuk", "v2zzdemus", "v2zzdemvz")
numvars <- vars[vars != "v2zzmaterials"]

# We only want to update the PSQ when the year is the same as right now
last_mod_year <- to_year(as.Date(file.info(FILEPATH)$ctime))
stopifnot(`Update PSQ file first!` = last_mod_year == to_year(as.Date(Sys.time())))

psq_data <- read_file(FILEPATH)

df <- psq_data %>%
    subset(name %in% vars) %>%
    arrange(coder_id, question_id, desc(version), desc(rating_id), country_id) %>%
    distinct(coder_id, question_id, .keep_all = TRUE) %>%
    arrange(name, coder_id)

df %>%
    group_by(name) %>%
    summarize(n_code = sum(!is.na(code), na.rm = TRUE),
        n_text_answer = sum(!is.na(text_answer) & text_answer != "", 
        na.rm = TRUE)) %>%
    untibble

df %<>%
    select(name, coder_id, code, text_answer, version) %>%
    mutate(text_answer = ifelse(!is.na(code), code, text_answer)) %>%
    select(-code) %>%
    arrange(name, coder_id, desc(version)) %>%
    distinct(name, coder_id, .keep_all = TRUE) %>%
    select(-version)

df_wide <- df %>%
    long_to_wide(., id_vars = c("coder_id"), id_var = "name", 
        value_var = "text_answer")

df_wide[,] <- Map(function(v, nn) {
    if (nn %in% numvars) {
        return(as.integer(v))
    }
    return(v)
}, v = df_wide, nn = names(df_wide))

write_file(df_wide, file.path(Sys.getenv("ROOT_DIR"), "dataset", 
    "v2zz_coder_level.rds"), dir_create = TRUE)

