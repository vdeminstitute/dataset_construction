#!/usr/bin/env Rscript

# Test script like this:
# export TASK_ID=7284; export MODULE_NAME=prep_accountability; export VARIABLE=prep_accountability; zsh -c 'Rscript ./R/accountability/prep_accountability.R -- 2>&1 | tee /dev/tty | xargs -d "\n" log_postgres; echo "${pipestatus[1]}"'

suppressMessages(library(vutils))
logs <- commandArgs()
ind <- which(grepl("--args", logs, fixed = TRUE))
logs <- logs[(ind + 1):length(logs)]

task_id <- as.numeric(Sys.getenv("TASK_ID"))
module_name <- Sys.getenv("MODULE_NAME")
question_name <- Sys.getenv("VARIABLE")

db <- pg_connect(dbname = Sys.getenv("DS_VERSION"))

df <- data.frame(task_id = task_id,
	question_name = question_name,
	module_name = module_name,
	logmsg = logs)
pg_append_table(df, "pipe.logs", db)
