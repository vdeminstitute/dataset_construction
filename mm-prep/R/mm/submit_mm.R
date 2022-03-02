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
stopifnot(is_path_mounted())

# copy mm file to mounted HPC directory
objs <- find_dep_files(Sys.getenv("TASK_ID"), db)
write_file(objs[[varname]], file.path(Sys.getenv("MM_SSH_DIR"), "mm", varname %^% ".rds"))

timeout <- "12:00:00"

# Some of these variables may take unusually long so we increase the timeout
if (varname %in% c("v2cldiscm", "v2caviol", "v2pepwrses", 
				   "v2cacamps", "v2merange", "v2clacjstw", 
				   "v2caassemb", "v2csprtcpt", "v2csantimv", 
				   "v2cagenmob", "v2lgcrrpt")) {
	timeout <- "96:00:00"				   
}

# submit job
mm_submit_XXXXX_job(variable = varname,
                        iter = 10000L,
                        timeout = timeout,
                        directory = Sys.getenv("MM_DIR"))

update_task_status(db = db)
lock_task(db = db)
