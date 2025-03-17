#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# This script reads log files from measurement models jobs and decides based
# on logged output whether or not a job should be 1) fetched as a result or 2)
# resubmitted at a higher iteration count.
#
# The status is updated based on parsing log files.
# --------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()

# Check if dependencies are done
check_direct_deps(id = Sys.getenv("VPIPE_TASK_ID"), db = db)

# Check if directory is mounted to HPC
stopifnot(is_path_mounted())

# ITER_OVERRIDE
ITER_OVERRIDE <- as.integer(Sys.getenv("ITER"))
# -- if not set: FALSE -> ITER_OVERRIDE
# -- if set: ITER -> ITER_OVERRIDE
ITER_OVERRIDE <- ifelse(is.na(ITER_OVERRIDE), FALSE, ITER_OVERRIDE)

# MM_OVERRIDE
MM_OVERRIDE <- isTRUE(as.logical(Sys.getenv("MM_OVERRIDE")))

# MIN_ITERATION 
MIN_ITERATION <- suppressWarnings(as.integer(Sys.getenv("MIN_ITERATION")))
MIN_ITERATION <- ifelse(is.na(MIN_ITERATION), 0L, MIN_ITERATION)
stopifnot(!is.na(MIN_ITERATION))

# MM_CEILING_ITERATION 
MM_CEILING_ITERATION <- suppressWarnings(as.integer(Sys.getenv("MM_CEILING_ITERATION")))

# Settings:
info(sprintf("Running status_mm with settings: [ITER_OVERRIDE: %s] [MM_OVERRIDE: %s] [MIN_ITERATION: %d] [MM_CEILING_ITERATION: %d]", ITER_OVERRIDE, MM_OVERRIDE, MIN_ITERATION, MM_CEILING_ITERATION))

# Parse HPC log files per variable
df <- mm_log_table(file.path(Sys.getenv("MM_SSH_DIR"), "logs"), TASK_NAME)

# From the logfile (df)
# -- If there are any converged jobs, always proceed
# -- If there are no converged jobs, evaluate ITER_OVERRIDE and MM_OVERRIDE
# -- If either ITER_OVERRIDE or MM_OVERRIDE are TRUE, proceed
# -- -- If ITER_OVERRIDE is anything but 0, it will be TRUE
# -- 1) if both ITER_OVERRIDE and MM_OVERRIDE are TRUE, then MM_OVERRIDE takes precedence
# If any are converged, grab the model, download the file locally and finish script.
if (any(df$status == "converged") | as.logical(ITER_OVERRIDE) | MM_OVERRIDE) {

    # Choose converged version
    iter_df <-
        filter(df, status == "converged") %>%
        arrange(desc(iter)) %>%
        filter(row_number() == 1) %>%
        select(iter, logfile) %>%
        mutate(iter = as.numeric(iter))
    
    # ITER_OVERRIDE?
    if(as.logical(ITER_OVERRIDE)) {
        info("Evaluating using ITER_OVERRIDE")
        iter_df <-
            filter(df, status %in% c("warning", "converged", "failed")) %>%
            mutate(iter = as.numeric(iter)) %>%
            # There is commonly only one run per iteration count.
            filter(iter == ITER_OVERRIDE) %>%
            filter(row_number() == 1) %>%
            select(iter, logfile)
        stopifnot(nrow(iter_df) == 1)
        info(sprintf("Iterations chosen for [%s] with status [%s]: [%d]",
            TASK_NAME, iter_df$status, iter_df$iter))
    }

    # MM_OVERRIDE?
    if (MM_OVERRIDE) {
        info("Evaluating using MM_OVERRIDE")
        iter_df <-
            filter(df, status %in% c("warning", "converged", "failed")) %>%
            mutate(order_item = case_when(
                status == "converged" ~ 1,
                status == "warning" ~ 2,
                status == "failed" ~ 3)) %>%
            arrange(order_item, desc(iter)) %>%
            filter(row_number() == 1) %>%
            select(iter, logfile) %>%
            mutate(iter = as.numeric(iter))
        stopifnot(nrow(iter_df) == 1)
        info(sprintf("Iterations chosen for [%s] with status [%s]: [%d]",
            TASK_NAME, iter_df$status, iter_df$iter))
    }

    stopifnot(`More than one job selected` = nrow(iter_df) == 1)
    # Load file
    ll <- read_file(file.path(Sys.getenv("MM_SSH_DIR"), "out",
            paste0(iter_df$iter/1000, "k"), TASK_NAME %^% ".rds"))

    # Print log file so that it gets sent to postgres logs table
    logs <- trimws(readLines(iter_df$logfile), which = "both")
    logs <- logs[logs != ""]
    print(shQuote(logs))
    ll$logs <- logs

    write_file(ll, OUTFILE, dir_create = T)
    lock_task(db = db)
    quit(save = "no", status = 0L)
}

# Define max_iter as the largest iteration count for a previously run job
# -- these jobs cannot have status 'cancelled' or 'error'.
max_iter <-
    filter(df, !status %in% c("cancelled", "error")) %>%
    mutate(iter = as.integer(iter)) %$% max(iter)
stopifnot(`Cannot get iteration count...` = !is.na(max_iter))

# Evaluate max_iter, MIN_ITERATION, and job status:
# -- If you have a running job, but you pro-actively want to submit a new job at higher iteration, set MIN_ITERATION to something that is larger than the current maximum iteration count for that variable.
# Conditions that leads to re-submitting:
# -- if all existing jobs are failed, cancelled, error, timed_oud, warning -> Submit a new job
# -- if MIN_ITERATION is larger than max_iter -> Submit a new job

if (all(df$status %in% c("failed", "cancelled", "error", "timed_out", "warning")) | 
        (MIN_ITERATION > max_iter)) { 
    # If the condition is TRUE, we continue with the script below
} else {
    # IF the condition is FALSE, we exit with status 3
    info("Waiting for hpc...")
    quit(save = "no", status = 3L)
}

info(sprintf("Submitting at higher iteration for %s", TASK_NAME))

if (max_iter == 10000L & MM_CEILING_ITERATION >= 20000L) {
    mm_submit_tetralith_job(variable = TASK_NAME,
                            iter = 20000L,
                            timeout = "48:00:00",
                            directory = Sys.getenv("MM_DIR"))
}

if (max_iter == 20000L & MM_CEILING_ITERATION >= 40000L) {
    mm_submit_tetralith_job(variable = TASK_NAME,
                            iter = 40000L,
                            timeout = "90:00:00",
                            directory = Sys.getenv("MM_DIR"))
}

if (max_iter == 40000L & MM_CEILING_ITERATION >= 80000L) {
    mm_submit_tetralith_job(variable = TASK_NAME,
                            iter = 80000L,
                            timeout = "120:00:00",
                            directory = Sys.getenv("MM_DIR"))
}

if (max_iter == 80000L & MM_CEILING_ITERATION >= 200000L) {
    mm_submit_tetralith_job(variable = TASK_NAME,
                            iter = 200000L,
                            timeout = "160:00:00",
                            directory = Sys.getenv("MM_DIR"))
}

quit(save = "no", status = 3L)