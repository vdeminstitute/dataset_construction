#!/usr/bin/env Rscript
#
# Downloads all reference tables as is
###


options(warn = 2)
suppressMessages(library(vutils))
suppressMessages(library(vanalysis))
suppressMessages(library(docopt))
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))

# Connect to database to track tasks
db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
OUTDIR <- file.path(ROOT, "download")
# Connect to database for data download
db_data <- pg_connect(Sys.getenv("DOWNLOAD_DB"))

info("Starting downloads...")

tbls <- c("codebook", "question", "choice",
          "country", "country_list", "country_year_mask", "vignette_parent",
          "coder", "country_succession", "country_unit", "question_type",
          "survey", "question_queue", "vignette_pairs", "codebook_org", "task",
          "question_queue", "dichotomised_ids", "lateral_dates")

lapply(tbls, function(table_name) {
    info("Writing table: " %^% table_name)
    DBI::dbGetQuery(db_data, paste0("SELECT * FROM ", table_name, ";")) %>%
        write_file(file.path(OUTDIR, table_name %^% ".rds"), dir_create = T)
}) %>% invisible

# Static pointer to ROOT_DIR (used for STATA scripts)
system("ln -nfs " %^% Sys.getenv("ROOT_DIR") %^% " ~/proj/mm-prep/ROOT")

update_task_status(db = db)