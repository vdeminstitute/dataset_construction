# !/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# This script is used to submit a job to the HPC. It:
# 1) copies the input file to the cluster
# -- a) this relies on that the cluster is mounted 
# 2) chooses iter and timeout based on previous run times and convergence status
# 3) sends a request to run the job

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# function
# ------------------------------------------------------------------------------
check_endtime <- function(cur_time, timeout) {
    old_timeout <- as.numeric(strsplit(timeout, ":")[[1]][1])
    end_time <- as.POSIXct(strptime(cur_time, "%H:%M:%S")) + as.difftime(old_timeout, units = "hours")
    
    # check if end time is within allowed range (8:00 to 0:00)
    if (as.integer(format(end_time, "%H")) >= 0 & as.integer(format(end_time, "%H")) < 8) {
        info("End time is outside allowed range adjusting timeout.")
        add_hours = 8 - as.integer(format(end_time, "%H")) 
        new_timeout = sprintf("%02d:00:00", old_timeout + add_hours)
        # can't go above the max timout (120 hours)
        if (as.integer(substr(new_timeout, 1, 2)) > 120) {
            new_timeout <- timeout
        }
    } else {
        info("End time is within allowed range.")
        new_timeout <- timeout
    }
    return(new_timeout)
}

# ------------------------------------------------------------------------------
# Globals and database connection
db <- pg_connect()
get_globals()
vdem_data <- pg_connect(Sys.getenv("DOWNLOAD_DB"))
cur_time <- Sys.time() %>% format("%H:%M:%S")

# Check if directory is mounted to HPC
stopifnot(is_path_mounted())

# copy mm file to mounted HPC directory
objs <- find_dep_files(TASK_ID, db)
write_file(
    objs[[TASK_NAME]],
    file.path(Sys.getenv("MM_SSH_DIR"), "mm", TASK_NAME %^% ".rds"))

# choose iter and timeout based on previous run times and convergence status
prev_info <- DBI::dbGetQuery(vdem_data, sprintf("SELECT * FROM mm_stats WHERE name = '%s'", TASK_NAME))

if (nrow(prev_info) == 0) {
    timeout <- "12:00:00"
    iter <- 10000L
} else {
    release = max(prev_info$version)
    iter <- filter(prev_info, version == (release))$iter
        if (length(iter) == 0L) {
            iter <- prev_info$iter[nrow(prev_info)]
        }
    iter <- case_when(
        iter == 200000L ~ 80000L,
        iter == 80000L  ~ 40000L,
        iter == 40000L  ~ 20000L,
        iter == 20000L  ~ 10000L,
        TRUE ~ iter)

    # choose timeout
    if (iter == 10000L) {
        # -- if iter is 10K, then we use the previous run time + 5 hours
        timeout <- (filter(prev_info, version == (release))$run_time) + 5 
    } else if (iter == 20000L) {
        # -- if iter is 20K, then we use the previous run time + 10 hours
        timeout <- (filter(prev_info, version == (release))$run_time) + 10 
    } else if (iter %in% c(40000L, 80000L)) {
        # -- if iter is 40K, then we use the previous run time + 15 hours
        timeout <- (filter(prev_info, version == (release))$run_time) + 15 
    } 

    timeout <- ceiling(timeout)

    if (timeout > 120) {
        timeout <- 120
    }

    timeout <- sprintf("%02d:00:00", as.integer(timeout))
} 

timeout <- check_endtime(cur_time, timeout)

info(sprintf("Submitting %s with iter %d and timeout %s", TASK_NAME, iter, timeout))

# submit job
mm_submit_tetralith_job(variable = TASK_NAME,
                        iter = iter,
                        timeout = timeout,
                        directory = Sys.getenv("MM_DIR"))

# We do not want to run this task again if the above job is successful
lock_task(db = db)