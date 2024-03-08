#!/usr/bin/env Rscript

# Download all reference documents that are used during V-Dem dataset construction
suppressMessages(library(vanalysis))
suppressMessages(library(docopt))
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(DBI))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Set global environment
get_globals()
# Database to download references documents from
db_data <- pg_connect(Sys.getenv("DOWNLOAD_DB"))
# Root directory for reference documents
OUTDIR <- file.path(ROOT, "download")
tbls <- c("codebook", "question", "choice", "country", "country_list", "country_year_mask", "vignette_parent", "coder", "country_succession", "country_unit", "question_type", "survey", "question_queue", "vignette_pairs", "codebook_org", "task", "question_queue", "dichotomised_ids", "lateral_dates")

info(sprintf("Downloading reference documents: %s", toString(tbls)))
invisible(lapply(tbls, function(table_name, outd) {
    info(sprintf("Downloading table: %s", table_name))
    write_file(
        o = dbGetQuery(conn = db_data, sprintf("SELECT * FROM %s;", table_name)),
        f = file.path(outd, paste0(table_name, ".rds")),
        dir_create = TRUE)
}, outd = OUTDIR))

info("All reference documents have successfully been downloaded.")