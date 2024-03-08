#!/usr/bin/env Rscript

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))
options(warn = 1)

# Global variables
db <- pg_connect()
get_globals()

ROOT <- Sys.getenv("ROOT_DIR")
CHECKS_DIR <- file.path(ROOT, "checks")

# Imports
df <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]$cy
df <- df[, !grepl("code[a-z]*95$", names(df))]
mm <- subset(load_qtable(), name == TASK_NAME)$mm

stopifnot(length(mm) == 1)
stopifnot(!all(grepl("68$", names(df)), grepl("95$", names(df))))

# country_text_id historical_date
df <- add_country_cols(add_date_cols(df), load_country())
# clean by utable
df <- clean_by_utable(df, load_country_unit())
write_file(df, OUTFILE, dir_create = TRUE)

# What mode should be plotted?
variable_type <- ifelse(
    isTRUE(mm), "mm",
    ifelse(grepl("^v2x", TASK_NAME), "indices", "rest"))

# Define arguments to ds_check
func_args <- c("./checks/ds_check.R",
	"--plots",
	"--v_df=" %^% OUTFILE,
	"--v_df2=" %^% Sys.getenv("OLD_DS_FILE"),
	"--variables=" %^% variable_type,
    "--outdir=" %^% CHECKS_DIR,
    "--rootdir=" %^% ROOT,
    "--ncores=1")

# Create plots
out <- suppressWarnings(system2(func_args[1], 
		args = func_args[-1], 
		stderr = TRUE, 
		stdout = TRUE))

# Capture exit code
if (is.null(attr(out, "status"))) {
    attr(out, "status") <- 0L
}

if (attr(out, "status") != 0) {
	quit(save = "no", status = 5)
}
