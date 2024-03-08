#!/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(vpipe))
suppressMessages(library(vutils))

get_globals()
db <- pg_connect()

destination <- file.path(ROOT, "accountability")
path <- Sys.getenv("ACC_SSH_DIR")
check_direct_deps(TASK_ID, db)
infile <- list.files(file.path(path, "out"), pattern = TASK_NAME, full.names = T)

# ------------------------------------------------------------------------------
# Read logs
logs <- list.files(file.path(path, "logs"), pattern = TASK_NAME, full.names = T)
stopifnot(`Cannot identify a unique log file, check HPC environment`
    = length(logs) == 1)
d <- readLines(logs)
print(shQuote(d))

if (any(grepl("Finished!", d, fixed = TRUE))) {

	if (length(infile) == 0) {
        stop("Cannot find any output file from job?", call. = FALSE)
	}
	
    if (length(infile) > 1) {
	    stop("Found more than one output file, check HPC directory", call. = FALSE)
	}

    ll <- read_file(infile)
    ll$logs <- d
    ll[[TASK_NAME]]$cy <- select(ll[[TASK_NAME]]$cy, country_text_id, year, matches("acc"))
    
    write_file(ll, OUTFILE, dir_create = TRUE)
    lock_task(db = db)
    # Exit successfully
    quit(save = "no", status = 0L)
}

# Exit with "error"
if (any(grepl("Error", d))) {
    quit(save = "no", status = 1L)
}

# Exit with "hpc_waiting"
quit(save = "no", status = 3L)
