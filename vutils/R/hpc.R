

#' @export
mm_submit_tetralith_job <- function(variable, iter, timeout = "60:00:00",
                    directory = Sys.getenv("MM_DIR")) {
    system("ssh tetralith " %^%
           "'cd " %^% directory %^% "; " %^%
           "export LOGDIR=" %^% directory %^% "/logs/; " %^%
           "export VARIABLENAME=" %^% variable %^% "; " %^%
           "export ITER=" %^% iter %^% "; " %^%
           "export R_SEED=0; " %^%
           "export STAN_SEED=0; " %^%
           "./scripts/zbatch " %^%
                "-t " %^% timeout %^%
                " scripts/mm_batch.sh " %^% variable %^%
           " '")
    info("Submitted " %^% variable %^% " with " %^% iter %^% " iterations.")
}


#' @export
bfa_submit_tetralith_job <- function(index,
    timeout = "24:00:00",
    directory = Sys.getenv("BFA_DIR"),
    script = "R/bfa.R",
    hpc = "tetralith") {
    system("ssh " %^% hpc %^% " " %^%
           "'cd " %^% directory %^% "; " %^%
           "export LOGDIR=" %^% directory %^% "/logs/; " %^%
           "export VARIABLENAME=" %^% index %^% "; " %^%
           "./scripts/zbatch " %^%
                "-t " %^% timeout %^%
                " scripts/bfa_batch.sh " %^% script %^% "'")
    info("Submitted " %^% index)
}

#' @export
mm_read_log <- function(logpath, local_dir = FALSE) {
	if (!local_dir) {
		stopifnot(is_path_mounted())
	}
	
    print(logpath)
    f <- readLines(logpath)
    if (any(grepl("file.exists(INFILE) is not TRUE", f, fixed = T)))
        return(NULL)

    # Create data.frame
    df <- data.frame(job_id = gsub("^.*[_](.*?)[.]out$", "\\1", basename(logpath)),
                     stringsAsFactors = F)

    # if there is an early error before we can parse any data we still want to
    # know that there was an error
    if (any(grepl("Error", f)) & !any(grepl("START", f))) {
        df$status <- "error"
        df$logfile <- logpath
        return(df)
    }

    df$name <- stringr::str_extract(f, "NAME[:]\\s.*$") %>% na.omit %>%
        gsub("NAME: ", "", ., fixed = T)
    df$iter <- stringr::str_match(f, "ITER[:]\\s\\d+") %>% na.omit %>%
        gsub("ITER: ", "", ., fixed = T)
    df$infile <- stringr::str_extract(f, "INFILE[:]\\s.*$") %>% na.omit %>%
        gsub("INFILE: ", "", ., fixed = T)
    df$outdir <- stringr::str_extract(f, "OUTDIR[:]\\s.*$") %>% na.omit %>%
        gsub("OUTDIR: ", "", ., fixed = T)
    df$start <- stringr::str_extract(f, "START[:]\\s.*$") %>% na.omit %>%
        gsub("START: ", "", ., fixed = T)
    df$r_seed <- stringr::str_extract(f, "R_SEED[:]\\s.*$") %>% na.omit %>%
        gsub("R_SEED: ", "", ., fixed = T)
    df$stan_seed <- stringr::str_extract(f, "STAN_SEED[:]\\s.*$") %>% na.omit %>%
            gsub("STAN_SEED: ", "", ., fixed = T)
    df$logfile <- logpath
    df$status <- dplyr::case_when(
        any(grepl("Convergence failed", f, fixed = TRUE)) ~ "failed",
        any(grepl("(Bulk|Tail) Effective Samples Size [(]ESS[)] is too low", f)) &
            any(grepl("Done with", f)) ~ "warning",
        any(grepl("Done with", f)) ~ "converged",
        any(grepl("Error", f)) ~ "error",
        any(grepl("Chain", f[length(f)])) ~ "running",
        any(grepl("TIME LIMIT", f)) ~ "timed_out",
        any(grepl("CANCELLED", f)) ~ "cancelled",
        TRUE ~ "other")
    if(length(stringr::str_extract(f, "END[:]\\s.*$") %>% na.omit) > 0) {
        df$end <-
            stringr::str_extract(f, "END[:]\\s.*$") %>%
            na.omit %>%
            gsub("END: ", "", ., fixed = T) %>%
            .[1]
    }
	if (df$status == "cancelled") {
		df$end <- 
			stringr::str_extract(f, "CANCELLED\\sAT\\s.*$") %>% 
			na.omit %>%
			gsub("CANCELLED AT ", "", ., fixed = TRUE) %>%
			gsub(" ***", "", ., fixed = TRUE) %>%
			gsub("T", " ", ., fixed = TRUE)

	}
    df %<>% dplyr::select(job_id, name, iter, status, start, dplyr::everything())
    return(df)
}

#' @export
mm_log_table <- function(LOG_PATH, varname) {
    ll <- list.files(LOG_PATH, full.names = T, pattern = paste0(varname, "-")) %>%
        lapply(mm_read_log) %>%
        dplyr::bind_rows() %>%
        dplyr::arrange(name)
    return(ll)
}

#' @export
mm_log_table_all <- function(LOG_PATH) {
    ll <- list.files(LOG_PATH, full.names = T)
    dplyr::bind_rows(lapply(ll, mm_read_log)) %>% dplyr::arrange(name)
}

#' @export
make_slurm_fname <- function(file_name) {
  slurm_job_id <- Sys.getenv("SLURM_JOB_ID")
  slurm_node_id <- Sys.getenv("SLURM_JOB_NODELIST")
  slurm_name <- paste(slurm_job_id, slurm_node_id, file_name, sep = "_")
  return(slurm_name)
}

#' @export
submit_accountability <- function(v, directory = Sys.getenv("ACC_DIR"),
    timeout = "72:00:00", script = "scripts/submit.sh",
        hpc = "tetralith") {

    stopifnot(!is.null(v), length(v) == 1)
    if (hpc == "tetralith") {
        ssq <- shQuote("projinfo -C | grep snic | awk '{print $3}'")
        account <- system(paste0("ssh ", hpc, " ", ssq), intern = TRUE)
    }
    if (hpc == "kebnekaise")
        account <- "SNIC2019-3-154"

    logfile <- paste0(directory, "/logs/", v, "-", Sys.Date(), "_%A_%N.out")
    rscript <- paste0(directory, "/R/", v, ".R")


    system(paste0("ssh ", hpc, " 'cd ", directory,
        "; export var=", v,
        "; export rscript=", rscript,
        "; sbatch -J ", v, " -A ", account," -o ", logfile,
        " -e ", logfile, " --time=", timeout,
        " --mail-type=FAIL -N 1 --exclusive ", script, "'"))
}

#' @export
squeue_table <- function(USER = "x_johvo") {
    system(paste0("ssh tetralith \"squeue -u ", USER, 
                  " --format='%.18i %.9P %.20j %.8u %.2t %.10M %.6D %R'\""), 
           intern = TRUE) %>%
    strsplit(., " ") %>% 
    lapply(function(v) {
        v <- v[v != ""]
        data.frame(
            job_id =  v[1],
            hpc = v[2],
            var_short = v[3],
            user = v[4],
            run_status = v[5],
            run_time = v[6],
            nodes = v[7],
            nodelist_reason = v[8])
    }) %>% dplyr::bind_rows(.) %>% .[-1, ]
}

#' @export
mm_summary <- function(df_logs) {
    df_logs %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(status = dplyr::case_when(
        any(status == "converged") ~ "converged",
        any(status == "warning") ~ "warning",
        any(status == "running") ~ "running",
        any(status == "other") ~ "other",
        any(status == "failed") ~ "failed",
		any(status == "timed_out") ~ "timed_out",
        TRUE ~ "weird"), .groups = "drop") %>%
    dplyr::arrange(name, status) %$%
    table_(status)
}