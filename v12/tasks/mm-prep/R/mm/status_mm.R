#!/usr/bin/env Rscript
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(vutils))

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
OUTFILE <- create_outfile(id = Sys.getenv("TASK_ID"),
                          ROOT = Sys.getenv("ROOT_DIR"),
                          db = db)
varname <- get_varname(id = Sys.getenv("TASK_ID"), db)
VARNAME <- varname
check_direct_deps(id = Sys.getenv("TASK_ID"), db)

# Check if directory is mounted to HPC
stopifnot(is_path_mounted())

ITER_OVERRIDE <- as.integer(Sys.getenv("ITER"))
ITER_OVERRIDE <- ifelse(is.na(ITER_OVERRIDE), FALSE, ITER_OVERRIDE)

MM_OVERRIDE <- isTRUE(as.logical(Sys.getenv("MM_OVERRIDE")))
MIN_ITERATION <- suppressWarnings(as.integer(Sys.getenv("MIN_ITERATION")))
MIN_ITERATION <- ifelse(is.na(MIN_ITERATION), 0L, MIN_ITERATION)
stopifnot(!is.na(MIN_ITERATION))

# Parse log files per variable
df <- mm_log_table(file.path(Sys.getenv("MM_SSH_DIR"), "logs"), varname)

if (any(df$status == "converged") | as.logical(ITER_OVERRIDE) | MM_OVERRIDE) {

    # Choose converged version with highest number of iterations.
    iter_df <-
        df %>%
        filter(status == "converged") %>%
        arrange(desc(iter)) %>%
        filter(row_number() == 1) %>%
        select(iter, logfile) %>%
        mutate(iter = as.numeric(iter))
    
    # Applying ITER_OVERRIDE
    if(as.logical(ITER_OVERRIDE)) {
        iter_df <-
            df %>%
            filter(status %in% c("warning", "converged", "failed")) %>%
            mutate(iter = as.numeric(iter)) %>%
            # There is commonly only one run per iteration count.
            filter(iter == ITER_OVERRIDE) %>%
            filter(row_number() == 1) %>%
            select(iter, logfile)
        info("Iterations chosen for variable: " %^% iter_df$iter)
    }

    # Applying MM_OVERRIDE
    if (MM_OVERRIDE) {
        iter_df <-
            df %>%
            filter(status %in% c("warning", "converged", "failed")) %>%
            mutate(order_item = case_when(
                status == "converged" ~ 1,
                status == "warning" ~ 2,
                status == "failed" ~ 3)) %>%
            arrange(order_item, desc(iter)) %>%
            filter(row_number() == 1) %>%
            select(iter, logfile) %>%
            mutate(iter = as.numeric(iter))
    }

    stopifnot(`Not exactly one run chosen for this variable` = 
        {nrow(iter_df) == 1})
    # Load file
    ll <- read_file(file.path(Sys.getenv("MM_SSH_DIR"),
              "out", paste0(iter_df$iter/1000, "k"), varname %^% ".rds"))

    # Print log file so that it gets sent to postgres logs table
    logs <- readLines(iter_df$logfile)
    logs <- logs %>% trimws(which = "both") %>% .[. != ""]
    print(shQuote(logs))
    ll$logs <- logs

    write_file(ll, OUTFILE, dir_create = T)
    update_task_status(db = db)
    lock_task(db = db)
    quit(save = "no", status = 0)
}

max_iter <-
    df %>%
    mutate(iter = as.integer(iter)) %$% max(iter)
stopifnot(`Cannot get iteration count...` = !is.na(max_iter))


if (all(df$status %in% c("failed", "cancelled", 
						 "error", "timed_out")) | 
        (MIN_ITERATION > max_iter)) { 
    
} else {
    info("Waiting for hpc...")
    update_task_status(status = "hpc_waiting", db = db)
    quit(save = "no", status = 0)
}

info(VARNAME %^% " submit at higher iteration!")

if (max_iter == 10000L) {
    mm_submit_XXXX_job(variable = varname,
                            iter = 20000L,
                            timeout = "48:00:00",
                            directory = Sys.getenv("MM_DIR"))
}

if (max_iter == 20000L) {
    mm_submit_XXXX_job(variable = varname,
                            iter = 40000L,
                            timeout = "90:00:00",
                            directory = Sys.getenv("MM_DIR"))
}

if (max_iter == 40000L) {
    mm_submit_XXXX_job(variable = varname,
                            iter = 80000L,
                            timeout = "120:00:00",
                            directory = Sys.getenv("MM_DIR"))
}

if (max_iter == 80000L) {
    mm_submit_XXXX_job(variable = varname,
                            iter = 200000L,
                            timeout = "160:00:00",
                            directory = Sys.getenv("MM_DIR"))
}

update_task_status(status = "hpc_waiting", db = db)
