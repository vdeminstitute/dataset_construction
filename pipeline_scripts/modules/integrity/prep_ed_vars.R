#!/usr/bin/env Rscript
# --------------------------------------------------------------------------
# prep mm objects from demed that go into electoral integrity index

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# --------------------------------------------------------------------------
db <- pg_connect()
get_globals()

if (grepl("^v2ed", TASK_NAME)) {
    fp <- list.files(path = "~/data/ds_construction/v12_ed_v2/mm/super",
        pattern = TASK_NAME,
        full.names = TRUE)
} else {
    fp <- list.files(path = "~/data/ds_construction/v14/mm/super",
        pattern = TASK_NAME,
        full.names = TRUE)
}

stopifnot(length(fp) == 1)

mm <- read_file(fp)

write_file(mm, OUTFILE)
