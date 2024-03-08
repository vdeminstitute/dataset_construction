
suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(vutils))

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
varname <- get_varname(id = Sys.getenv("TASK_ID"), db)
objs <- find_dep_files(Sys.getenv("TASK_ID"), db)

# Check if directory is mounted to HPC
stopifnot(is_path_mounted())

write_file(objs[[varname]],
    file.path(Sys.getenv("BFA_SSH_DIR"), "z.sample", varname %^% ".rds"),
    dir_create = T)


# Set timeout limit depending on number of components of the index!
# If we set a high timeout for all jobs XXXXXX will punish
# us in scheduling because it always assumes we will use all 
# the requested cpu-hours per job.
n_comps <- objs[[varname]][[varname]]$components %>% length
tim <- case_when(
    n_comps <= 6 ~ "48:00:00",
	n_comps <= 10 ~ "96:00:00",
    TRUE ~ "120:00:00")

info("Timeout chosen: " %^% tim)

# submit job
bfa_submit_XXXX_job(index = varname,
                         timeout = tim,
                         script = "R/bfa.R",
                         directory = Sys.getenv("BFA_DIR"),
                         hpc = "XXXXXX")
update_task_status(db = db)
lock_task(db = db)
