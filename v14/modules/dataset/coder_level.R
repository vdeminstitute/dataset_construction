#!/usr/bin/env Rscript
# ==========================================================================
# Creates the coder-level-dataset with
# interpolated raw coder scores,
# beta scores from the measurement model,
# and some Post Survey Questionnaire (PSQ) data.
# ==========================================================================

suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(parallel))
suppressMessages(library(tidyr))
suppressMessages(library(vanalysis))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()
ROOTDIR <- ROOT
DS_VERSION <- Sys.getenv("DS_VERSION")
qtable <- load_qtable()
tasks <- load_tasks(db)
modules <- load_modules(db)
utable <- load_country_unit()
country <- load_country()

# ------------------------------------------------------------------------------
# Read ZZ data
zz_df <- read_file(file.path(ROOT, "dataset", "v2zz_coder_level.rds"))

# File paths for interpolated:
inter_vars <- 
    subset(tasks, task_name != "v2test" & module_name == "interpolate_coders")$task_id
file_list_inter <- create_outfile_local(inter_vars, tasks, modules, ROOT)

# File paths for mm files
mm_vars <- 
    subset(tasks, task_name != "v2test" & task_name %!~% "_rec$" |
        task_name == "v2exdfcbhs_rec") %>%
    subset(module_name == "status_mm") %$% task_id
file_list_mm <- create_outfile_local(mm_vars, tasks, modules, ROOT)

# Load interpolated files and merge them into a data.frame
options(warn=1)
info("Merging interpolated coder-level data")
prep.ll <- mclapply(file_list_inter, function(f) {

    # Keep if DEMED are still included in the dataset
    ttable <- data.frame(
        old = c("v2edscpatriotceleb",
            "v2edideolcharctrx_rec",
            "v2edideolcharctrx",
            "v2xed_indcon",
            "v2xed_ed_patr",
            "v2xed_patr",
            "v2xed_ed_inst",
            "v2xedvd_inst",
            "v2xed_indconpatr"),
        new = c("v2edscpatriotcb",
            "v2edideolch_rec",
            "v2edideolch",
            "v2xed_ed_dmcon",
            "v2xed_ed_ptcon",
            "v2xed_ptcon",
            "v2xed_ed_inpt",
            "v2xedvd_inpt",
            "v2xed_ed_con"))

    rename_vars <- function(v) {
        for (i in seq_along(ttable$old)) {
            v <- gsub(paste0(ttable$old[i], "_"), paste0(ttable$new[i], "_"), 
                v, fixed = TRUE)
            v <- gsub(paste0(ttable$old[i], "$"), paste0(ttable$new[i]), v)
        }
        return(v)
    }

    ll <- read_file(f)
    varname <- names(ll)
    info("Merging in " %^% varname)
    input.data <- ll[[varname]]
    text_id <- rownames(input.data$wdata) %>% get_text_id
    dates <- rownames(input.data$wdata) %>% get_date

    # Subtract 1 from mm variables that are not percentage variables
    if(!(varname %in% qtable$name &&
        qtable$question_type[qtable$name == varname] == "R")) {
        input.data$wdata <- input.data$wdata - 1
    }

    wdata <-
        mutate(as.data.frame(input.data$wdata), country_text_id = text_id, historical_date = dates) %>%
        select(country_text_id, historical_date, everything()) %>%
        as.data.table(.) %>%
        data.table::melt(id.vars = c("country_text_id", "historical_date"),
            measure.vars = 3:ncol(.),
            variable.name = "coder_id",
            variable.factor = FALSE,
            na.rm = TRUE)
    wdata <- as.data.frame(wdata)
    wdata$coder_id <- as.numeric(wdata$coder_id)

    conf <-
        mutate(as.data.frame(input.data$conf_mat), country_text_id = text_id, historical_date = dates) %>%
        select(country_text_id, historical_date, everything()) %>%
        as.data.table(.) %>%
        data.table::melt(id.vars = c("country_text_id", "historical_date"),
            measure.vars = 3:ncol(.),
            variable.name = "coder_id",
            variable.factor = FALSE,
            na.rm = TRUE)
    conf <- as.data.frame(conf)
    conf$coder_id <- as.numeric(conf$coder_id)    

    colnames(wdata)[colnames(wdata) == "value"] <- rename_vars(varname)
    colnames(conf)[colnames(conf) == "value"] <- rename_vars(varname) %^% "_conf"

    full_join(wdata, conf,
        by = c("country_text_id", "historical_date", "coder_id")) %>%
    wide_to_long(id_vars = c("country_text_id", "historical_date", "coder_id"))

}, mc.cores = 8, mc.preschedule = FALSE)

raw.df <- rbindlist(prep.ll)
raw.df %<>% as.data.frame %>% mutate(coder_id = as.numeric(coder_id))

# Make sure that converting to/from factors to not mess with coder_id, i.e introducing a 1
stopifnot(!1 %in% raw.df$coder_id)
stopifnot(!anyNA(raw.df$variable))

# Add some beta (reliability) scores
info("Merging beta scores from the MM")
beta_files <- file_list_mm
stopifnot(length(beta_files) > 0)

beta.ll <- mclapply(beta_files, function(f) {

    # Keep in as long as DEMED is included
    ttable <- data.frame(
        old = c("v2edscpatriotceleb",
            "v2edideolcharctrx_rec",
            "v2edideolcharctrx",
            "v2xed_indcon",
            "v2xed_ed_patr",
            "v2xed_patr",
            "v2xed_ed_inst",
            "v2xedvd_inst",
            "v2xed_indconpatr"),
        new = c("v2edscpatriotcb",
            "v2edideolch_rec",
            "v2edideolch",
            "v2xed_ed_dmcon",
            "v2xed_ed_ptcon",
            "v2xed_ptcon",
            "v2xed_ed_inpt",
            "v2xedvd_inpt",
            "v2xed_ed_con"))

    rename_vars <- function(v) {
        # v <- names(ds_cy)
        for (i in seq_along(ttable$old)) {
            v <- gsub(paste0(ttable$old[i], "_"), paste0(ttable$new[i], "_"), 
                v, fixed = TRUE)
            v <- gsub(paste0(ttable$old[i], "$"), paste0(ttable$new[i]), v)
        }
        return(v)
    }

    ll <- read_file(f)
    varname <- names(ll)[1]
    info("Merging in beta file for: " %^% varname)
    b.df <-
        ll[[varname]]$b.summary %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        # -- posterior median of reliability scores
        select(coder_id, median)
    colnames(b.df)[colnames(b.df) == "median"] <- rename_vars(varname) %^% "_beta"
    if("coder_id" %in% b.df$coder_id) {
        stop("Look at the csv files")
    }

    return(b.df)
}, mc.cores = 8, mc.preschedule = FALSE)

betas <-
    Reduce(partial(full_join, by = "coder_id"), beta.ll) %>%
    mutate(coder_id = as.numeric(coder_id)) %>%
    as.data.frame(stringsAsFactors = FALSE)
stopifnot(no_duplicates(betas, cols = "coder_id"))
df_betas <-
    wide_to_long(betas, id_vars = "coder_id")
df_betas[["value"]] <- round(as.numeric(df_betas[["value"]]), 3)    

df <- long_to_wide(raw.df,
                   id_vars = c("country_text_id", "historical_date", "coder_id"),
                   id_var = "variable",
                   value_var = "value") %>%
    mutate(coder_id = as.numeric(coder_id))

# merge in betas
df_betas_wide <- long_to_wide(df_betas,
                              id_vars = c("coder_id"),
                              id_var = "variable",
                              value_var = "value")
out <- merge(df, df_betas_wide, by = "coder_id", all.x = TRUE)
stopifnot(nrow(out) == nrow(df))
stopifnot(!anyNA(out$historical_date))
stopifnot(!anyNA(out$country_text_id))

inRow <- nrow(out)
out %<>%
    mutate(year = to_year(historical_date)) %>%
    left_join(select(country, country_id, country_text_id),
        by = "country_text_id") %>%
    clean_by_utable(utable) %>%
    select(-year) %>%
    select(country_text_id, country_id, historical_date, coder_id, everything()) %>%
    organiseRows(country_text_id, coder_id, historical_date) %>% 
    left_join(zz_df, by = "coder_id")
stopifnot(inRow == nrow(out))
stopifnot(!anyNA(out$coder_id))

# ------------------------------------------------------------------------------
# Save datasets
coder_level_dir <- file.path(ROOTDIR, "dataset", "V-Dem-Coder-Level")
dir.create(coder_level_dir, showWarnings = FALSE)

fork1 <- mcparallel({
    info("Creating coder-level in CSV format")
    write_file(out, file.path(
        coder_level_dir, paste0("Coder-Level-Dataset-", DS_VERSION, ".csv")))
})

fork2 <- mcparallel({
    info("Creating coder-level in rds format")
    write_file(out, file.path(
        coder_level_dir, paste0("Coder-Level-Dataset-", DS_VERSION, ".rds")))
})

# Make sure there was no error in writing the files
res <- unlist(mccollect(
    list(fork1, fork2)))
# If there is an error, res does not contain null for that element
stopifnot(is.null(res))
