# !/usr/bin/env Rscript
# 
# Copy file to HPC and submit job
# HPC directory must be mounted!

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(vutils))

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
varname <- get_varname(id = Sys.getenv("TASK_ID"), db)

# Check if directory is mounted to HPC
mm <- system("mountpoint /mnt/XXXXXX", intern = TRUE)
stopifnot(isTRUE(attr(mm, "status") == 0) || grepl("is a mountpoint", mm))

# copy mm file to mounted HPC directory
objs <- find_dep_files(Sys.getenv("TASK_ID"), db)
write_file(objs[[varname]], file.path(Sys.getenv("MM_SSH_DIR"), "mm", varname %^% ".rds"))

# submit job
mm_submit_XXXXXX_job(variable = varname,
                        iter = 10000L,
                        timeout = "96:00:00",
                        directory = Sys.getenv("MM_DIR"))

update_task_status(db = db)
lock_task(db = db)
