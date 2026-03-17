#!/usr/bin/env Rscript
# --------------------------------------------------------------------------
# This script prepares the compare file used to generate mm_plots
# This file then is stored in mm/plot_prep and used to run ds_check.R

# The script also captures cases where CIs are non-overlapping CIs and where point estimates are outside of CIs

# The two main flags for face validity checks include:
# 1) Non-overlapping CIs between versions (red flag)
# 2) The point estimates are outside of new/old confidence interval (orange flag)

# The checks also flag any new or dropped country-years
# --------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))
options(warn = 1)

# Global variables
db <- pg_connect()
get_globals()

ROOT <- Sys.getenv("ROOT_DIR")
CHECKS_DIR <- file.path(ROOT, "checks")
OLD_DS_FILE <- read_file(Sys.getenv("OLD_DS_FILE"))
OLD_DS_VERSION <- Sys.getenv("DS_PREVIOUS_VERSION")
DS_VERSION <- Sys.getenv("DS_VERSION")
NEWEST_YEAR <- as.numeric(Sys.getenv("NEWEST_YEAR"))
ctable <- load_country() %>%
	distinct(country_id, country_text_id, name) %>%
	rename(country_name = name)

if (!dir.exists(file.path(CHECKS_DIR, "flags"))) {
	dir.create(file.path(CHECKS_DIR, "flags"))
} else {
	flags <- read_file(file.path(CHECKS_DIR, "flags", "flags.csv"))
}

# Imports
df <- find_dep_files(TASK_ID, db)[[TASK_NAME]][[TASK_NAME]]$cy

if (grepl("stock", TASK_NAME)) {
	df <- find_dep_files(TASK_ID, db)[[TASK_NAME]]$cy
}

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
    isTRUE(mm), "C",
    ifelse(grepl("^v2x", TASK_NAME), "D", "A"))

# Define arguments to ds_check
func_args <- c("./checks/face_validity_plots.R",
	"--plots",
	"--df_new=" %^% OUTFILE,
	"--df_old=" %^% Sys.getenv("OLD_DS_FILE"),
	"--vartype=" %^% variable_type,
    "--outdir=" %^% CHECKS_DIR,
    "--rootdir=" %^% ROOT,
    "--ncores=6")

# Create plots
out <- suppressWarnings(system2(func_args[1], 
		args = func_args[-1], 
		stderr = TRUE, 
		stdout = TRUE))

# Flag cases that need review
if (mm == TRUE) {
	var_name <- TASK_NAME %^% "_osp"
} else if (mm == FALSE) {
	var_name <- TASK_NAME
}

# next bit only applies if there are codelow/codehigh variables
if (any(grepl("codelow$", names(df)))) {

	var_codelow <- var_name %^% "_codelow"
	var_codehigh <- var_name %^% "_codehigh"

	df_old <- OLD_DS_FILE[!is.na(OLD_DS_FILE[[var_name]]), c("country_text_id", "year", grep(paste0("^",var_name), names(OLD_DS_FILE), value = TRUE))]
	df_old$version <- OLD_DS_VERSION

	df_new <- df[!is.na(df[[var_name]]), c("country_text_id", "year", grep(paste0("^",var_name), names(df), value = TRUE))]
	df_new$version <- DS_VERSION
	names(df_new)[grep("68", names(df_new))] <- gsub("68$", "", names(df_new)[grep("68$", names(df_new))])

	df_compare <- rbind(df_old, df_new) %>%
		left_join(ctable, by = "country_text_id") %>%
		group_by(country_text_id, year) %>%
			mutate(n_versions = n_distinct(version)) %>%
		ungroup() %>% 
		mutate(flag_dropped = ifelse(n_versions == 1 & version == OLD_DS_VERSION, TRUE, FALSE),
			flag_new = ifelse(n_versions == 1 & version == DS_VERSION & year != NEWEST_YEAR, TRUE, FALSE)) 

	if (nrow(filter(df_compare, flag_dropped == TRUE)) > 0) {
		info(sprintf("%s country-years dropped in new version found in %s", 
			nrow(filter(df_compare, flag_dropped == TRUE)), 
			paste(unique(filter(df_compare, flag_dropped == TRUE)$country_name), collapse = ", ")))
	}
	if (nrow(filter(df_compare, flag_new == TRUE)) > 0) {
		info(sprintf("%s new country-years in new version found in %s", 
			nrow(filter(df_compare, flag_new == TRUE)), 
			paste(unique(filter(df_compare, flag_new == TRUE)$country_name), collapse = ", ")))
	}

	df_overlap <- df_compare %>%
		long_to_wide(id_vars = c("country_text_id", "country_name", "year"),
			id_var = "version",
			value_var = c(var_name, var_codelow, var_codehigh))

	names(df_overlap) <- gsub(var_codehigh, "codehigh", names(df_overlap))
	names(df_overlap) <- gsub(var_codelow, "codelow", names(df_overlap))
	names(df_overlap) <- gsub(OLD_DS_VERSION, "old", names(df_overlap))
	names(df_overlap) <- gsub(DS_VERSION, "new", names(df_overlap))
	names(df_overlap) <- gsub(var_name, "var", names(df_overlap))

	df_overlap <- df_overlap %>%
		mutate(no_overlap = ifelse(codelow_new > codehigh_old | codehigh_new < codelow_old, TRUE, FALSE),
			outside_ci_new = ifelse((var_new < codelow_old | var_new > codehigh_old) & no_overlap == FALSE, TRUE, FALSE),
			outside_ci_old = ifelse((var_old < codelow_new | var_old > codehigh_new) & no_overlap == FALSE, TRUE, FALSE))

	# Red flag cases (no overlap)
	if (nrow(filter(df_overlap, no_overlap == TRUE)) > 0) {
		info(sprintf("%s countries with non-overlap found: %s", 
			length(unique(filter(df_overlap, no_overlap == TRUE)$country_name)),
			paste(unique(filter(df_overlap, no_overlap == TRUE)$country_name), collapse = ", ")))
	}

	# Orange flag cases (point estimate outside of CI)
	if (nrow(filter(df_overlap, outside_ci_new == TRUE | outside_ci_old == TRUE)) > 0) {
		info(sprintf("%s countries with point estimates outside of CI found: %s", 
			length(unique(filter(df_overlap, outside_ci_new == TRUE | outside_ci_old == TRUE)$country_name)), 
			paste(unique(filter(df_overlap, outside_ci_new == TRUE | outside_ci_old == TRUE)$country_name), collapse = ", "))) 
	}   

	if (nrow(filter(df_overlap, no_overlap == TRUE | outside_ci_new == TRUE | 
	outside_ci_old == TRUE)) > 0 | nrow(filter(df_compare, flag_dropped == TRUE)) > 0 | nrow(filter(df_compare, flag_new == TRUE)) > 0) {
		check <- data.frame()
		check1 <- data.frame()
		check2 <- data.frame()
		check3 <- data.frame()

		if (nrow(filter(df_compare, flag_dropped == TRUE)) > 0) {
			check <- df_compare %>%
				filter(flag_dropped == TRUE) %>%
				distinct(country_text_id, country_name, year) %>%
				mutate(flag = "dropped")
		}
		if (nrow(filter(df_compare, flag_new == TRUE)) > 0) {
			check1 <- df_compare %>%
				filter(flag_new == TRUE) %>%
				distinct(country_text_id, country_name, year) %>%
				mutate(flag = "new")
		}
		if (nrow(filter(df_overlap, outside_ci_new == TRUE | outside_ci_old == TRUE)) > 0) {
			check2 <- df_overlap %>%
				filter(outside_ci_new == TRUE | outside_ci_old == TRUE) %>%
				select(country_text_id, country_name, year) %>%
				mutate(flag = "outside")
		}
		if (nrow(filter(df_overlap, no_overlap == TRUE)) > 0) {
			check3 <- df_overlap %>%
				filter(no_overlap == TRUE) %>%
				select(country_text_id, country_name, year) %>%
				mutate(flag = "no overlap")
		}

		df_out <- rbind(check, check1, check2, check3) %>%
			mutate(var = TASK_NAME) %>%
			arrange(country_name, year) 

		if (exists("flags")) {
			df_out <- mutate(df_out, checked = NA, comments = NA)

			flags <- rbind(flags, df_out) %>%
				distinct(country_name, country_text_id, year, var, flag, checked, comments) %>%
				arrange(var, country_name, year)
		} else {
			flags <- df_out
		}

		write_file(flags, file.path(CHECKS_DIR, "flags", "flags.csv"), dir_create = TRUE)
	}
}

# Capture exit code
if (is.null(attr(out, "status"))) {
    attr(out, "status") <- 0L
}

if (attr(out, "status") != 0) {
	quit(save = "no", status = 5)
}
