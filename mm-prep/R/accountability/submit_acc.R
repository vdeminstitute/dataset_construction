#!/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(vutils))

db <- pg_connect(Sys.getenv("DS_VERSION"))
varname <- get_varname(id = Sys.getenv("TASK_ID"), db)
check_direct_deps(id = Sys.getenv("TASK_ID"), db)


submit_accountability(varname, timeout = "96:00:00", hpc = "XXXXXX")
info("waiting...")
Sys.sleep(3)

update_task_status(db = db)
lock_task(db = db)
