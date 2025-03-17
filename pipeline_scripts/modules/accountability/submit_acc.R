#!/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

get_globals()
db <- pg_connect()
check_direct_deps(TASK_ID, db)

vars <- c("v2x_horacc", "v2x_veracc", "v2x_diagacc")

if (TASK_NAME %in% vars) {
    timeout <- "48:00:00"
} else {
    timeout <- "72:00:00"
}

accountability_submit_job(TASK_NAME, timeout = timeout, hpc = "tetralith")
info("waiting...")
Sys.sleep(3)
lock_task(db = db)
