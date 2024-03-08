#!/usr/bin/env Rscript

options(warn = 2)
options(mc.cores = 6)

library(data.table)
library(dplyr)
library(dbplyr)
library(dtplyr)
library(magrittr)
library(parallel)
library(tidyr)
library(vanalysis)
library(vutils)

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
ROOTDIR <- ROOT
REFS   <- file.path(ROOTDIR, "refs")
qtable <- read_file(file.path(REFS, "question_table.rds"))
tasks <- tbl(db, in_schema("pipe", "make")) %>% collect(n = Inf)
modules <- tbl(db, in_schema("pipe", "modules")) %>% collect(n = Inf)
utable <- read_file(file.path(ROOT, "refs", "country_unit.rds"))
country <- read_file(file.path(ROOT, "refs", "country_table.rds"))

zz_df <- read_file(file.path(ROOT, "dataset", "v2zz_coder_level.rds"))


# vars for interpolated:
inter_vars <-
    tasks %>%
	filter(question_name != "v2test") %>%
    filter(module_name == "interpolate_coders") %$% task_id
file_list_inter <- create_outfile_local(inter_vars, tasks, modules, ROOT)

# vars for mm:
mm_vars <-
    tasks %>%
	filter(question_name != "v2test") %>%
    filter(question_name %!~% "_rec$" | question_name == "v2exdfcbhs_rec") %>%
    filter(module_name == "status_mm") %$% task_id
file_list_mm <- create_outfile_local(mm_vars, tasks, modules, ROOT)


# Load interpolated files and merge them into a data.frame
# Don't forget to add 1
info("Merging interpolated coder-level data")

prep.ll <- mclapply(file_list_inter, function(f) {
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
    # Using dtplyr here for more efficient memory usage
    wdata <-
        lazy_dt(input.data$wdata) %>%
        mutate(country_text_id = text_id, historical_date = dates) %>%
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
        lazy_dt(input.data$conf_mat) %>%
        mutate(country_text_id = text_id, historical_date = dates) %>%
        select(country_text_id, historical_date, everything()) %>%
        as.data.table(.) %>%
        data.table::melt(id.vars = c("country_text_id", "historical_date"),
            measure.vars = 3:ncol(.),
            variable.name = "coder_id",
            variable.factor = FALSE,
            na.rm = TRUE)
    conf <- as.data.frame(conf)
    conf$coder_id <- as.numeric(conf$coder_id)    

    colnames(wdata)[colnames(wdata) == "value"] <- varname
    colnames(conf)[colnames(conf) == "value"] <- varname %^% "_conf"

    full_join(wdata, conf,
              by = c("country_text_id", "historical_date", "coder_id")) %>%
    wide_to_long(id_vars = c("country_text_id", "historical_date", "coder_id"))
}, mc.cores = 6, mc.preschedule = FALSE)

raw.df <- rbindlist(prep.ll)
raw.df %<>% as.data.frame %>% mutate(coder_id = as.numeric(coder_id))

# We have had a bug with factors in the past, but we do not have a coder_id 1
stopifnot(!1 %in% raw.df$coder_id)





###
# Add some beta (reliability) scores
info("Merging beta scores from the MM")
beta_files <- file_list_mm
stopifnot(length(beta_files) > 0)

beta.ll <- mclapply(beta_files, function(f) {
    ll <- read_file(f)
    varname <- names(ll)[1]
    info("Merging in beta file for: " %^% varname)
    b.df <- ll[[varname]]$b.summary %>%
        as.data.frame(stringsAsFactors = FALSE) %>%
        select(coder_id, median)
    colnames(b.df)[colnames(b.df) == "median"] <- varname %^% "_beta"
    if("coder_id" %in% b.df$coder_id)
        stop("Look at the csv files")
    b.df
}, mc.cores = 6, mc.preschedule = FALSE)

betas <-
    Reduce(partial(full_join, by = "coder_id"), beta.ll) %>%
    lazy_dt %>%
    mutate(coder_id = as.numeric(coder_id)) %>%
    as.data.frame(stringsAsFactors = FALSE)

stopifnot(no_duplicates(betas, cols = "coder_id"))
df_betas <- wide_to_long(betas, id_vars = "coder_id")



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
out <- left_join_(df, df_betas_wide, by = "coder_id")
stopifnot(!anyNA(out$historical_date))
stopifnot(!anyNA(out$country_text_id))
out %<>%
    mutate(year = to_year(historical_date)) %>%
    left_join(select(country, country_id, country_text_id),
              by = "country_text_id") %>%
    clean_by_utable(utable) %>%
    select(-year) %>%
    select(country_text_id, country_id, historical_date, coder_id, everything()) %>%
    left_join_(zz_df, by = "coder_id")

dir.create(file.path(ROOTDIR, "dataset", "V-Dem-coder-level"), showWarnings = F)
fwrite(out,
       file.path(ROOTDIR, "dataset", "V-Dem-coder-level",
                 "coder_level_ds_v12.csv"))
                 
update_task_status(db = db)
