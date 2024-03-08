# !/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# This script is used to submit a job to the HPC. It:
# 1) copies the input file to the cluster
# -- a) this relies on that the cluster is mounted 
# 2) sends a request to run the job

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Globals and database connection
db <- pg_connect()
get_globals()

# Check if directory is mounted to HPC
stopifnot(is_path_mounted())

# copy mm file to mounted HPC directory
objs <- find_dep_files(TASK_ID, db)
write_file(
    objs[[TASK_NAME]],
    file.path(Sys.getenv("MM_SSH_DIR"), "mm", TASK_NAME %^% ".rds"))

# Set default timeout
timeout <- "12:00:00"
if (TASK_NAME %in% c(
    "v2cldiscm", "v2caviol", "v2pepwrses", 
	"v2cacamps", "v2merange", "v2clacjstw", 
	"v2caassemb", "v2csprtcpt", "v2csantimv", 
	"v2cagenmob", "v2lgcrrpt", "v2caautmob",
    "v2cademmob", "v2cafexch", "v2casurv",
    "v2clpspct", "v2elpdcamp", "v2exl_legitlead",
    "v2juhcind", "v2mheharjrn")) {

	timeout <- "96:00:00"
}

# submit job
mm_submit_tetralith_job(variable = TASK_NAME,
                        iter = 10000L,
                        timeout = timeout,
                        directory = Sys.getenv("MM_DIR"))

# We do not want to run this task again if the above job is successful
lock_task(db = db)