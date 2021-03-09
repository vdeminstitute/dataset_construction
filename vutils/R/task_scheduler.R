#' Run individual task
#'
#' @export
run_task <- function(id, module_name, question_name, cmd = "Rscript", script, 
    docopt = "", export = NA_character_, open_blas_single_threaded = TRUE) {
    if (is.na(id) | id == "NA") {
        stop("Wrong run_task call!")
    }

    # Environment variables for pipe.logs table
    envir_vars <- paste0(unlist(Map(function(var, value) {
                paste0("export ", var, "=", value, ";")
                }, var = c("TASK_ID", "MODULE_NAME", "VARIABLE"),
                c(id, module_name, question_name))), collapse = "")

    if (!is.na(export) && grepl("=", export) && grepl(";$", export)) {
        envir_vars <- paste(envir_vars, export, sep = "export ")
    } else if (!is.na(export)) {
        warning("Check export argument: does it have ; at the end? Does it contain =?")
    }

    if (open_blas_single_threaded) {
        envir_vars <- paste0(envir_vars, "export OPENBLAS_NUM_THREADS=1;")
    }

    end_string <- '2>&1 | tee /dev/tty | xargs -d "\n" log_postgres; echo "${pipestatus[1]}"'
    
    if (isTRUE(docopt == "" | is.na(docopt))) {
        func_args <- paste(cmd, script, end_string)
    } else {
        func_args <- paste(cmd, script, docopt, end_string)
    }
    func_args <- shQuote(func_args)
    system2("zsh", args = c('-c', func_args), env = envir_vars, stderr = TRUE, stdout = TRUE)
}

#' Determine which tasks need to be run
#'
#' @export
make <- function(ids,
                 deps = T,
                 force = F,
                 simulate = F,
                 n_cores = 4,
                 db) {

    make_tbl <-
        dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>%
        dplyr::collect(n = Inf)
    modules <-
        dplyr::tbl(db, dbplyr::in_schema("pipe", "modules")) %>%
        dplyr::collect(n = Inf)
    make_tbl <- dplyr::left_join(make_tbl,
                                 modules,
                                 by = c("module_id", "module_name"))

    # Remove or find dependencies
    if (!deps) {
        tasks <- dplyr::filter(make_tbl, task_id %in% ids)
    } else {
        info("Tracing dependencies...")
        tasks <- trace_deps(ids, make_tbl)
    }

    # What if we depend on a task that is locked or waiting?
    # We need to remove that branch and print a message
    info("Checking if tasks have unfinished dependencies...")
    # This function needs to be sped up using something like memoization
    # We shouldn't have to trace again and again the same tasks / branches.
    bool <- lapply(1:nrow(tasks), function(i) {
        deps <- trace_deps(tasks$task_id[i], make_tbl)
        if (nrow(deps) == 1) return(TRUE)
        # remove own value from deps
        deps <- dplyr::filter(deps, !task_id %in% tasks$task_id[i])

        ### Now that we're in a loop check for timestamps as well.
        current_timestamp <- tasks$ts[i]
        if (!is.na(current_timestamp) &
            (!tasks$status[i] %in% c("not_started", "recalc", "waiting"))) {
            if (any(deps$ts > current_timestamp) %||% FALSE)
                stop("Error, Timestamp issue, run check_timestamps(db)")
        }
        ### End check for timestamps


        # We still want to run if a dependency is set to waiting, but is in ids
        deps <- dplyr::filter(deps, !task_id %in% tasks$task_id)

        if (any(deps$status != "done")) {
            return(FALSE)
        }
        return(TRUE)
    }) %>% unlist

    if (any(!bool)) {
        info("These tasks are waiting: " %^% tasks$task_id[!bool])
        # Set waiting tasks to waiting and remove them from the tasks to do
        for (i in tasks$task_id[!bool]) {
            update_task_status_no_timestamp(
                status = "waiting",
                id = i,
                db = db)
        }

        tasks <- tasks[bool, ]
    }

    # Unless we force, we can remove those that are done
    if (!force) {
        tasks %<>% dplyr::filter(status != "done")
    }

    # Do not touch locked rows (change manually in postgres to get around this)
    tasks %<>% dplyr::filter(lock == FALSE)

    if (nrow(tasks) == 0L) {
        info("No tasks to do")
        return(NULL)
    }

    # For simulation just return table
    if (simulate) return(tasks)

    # In other cases call run_make to execute the tasks
    run_make(tasks, n_cores = n_cores, db)


}

#' Determine order of tasks to run and pass on to run_task function
#'
#'@export
run_make <- function (tasks, n_cores = 1, db) {
    # run all jobs whose id is not in deps
    # remove from table,
    # run all jobs whose id is not in deps
    # repeat
    counter <- 1

    while (T) {
        info("Loop of independent tasks: " %^% counter)

        subtasks <- tasks
        run_vars <- numeric(0)
        # which rows do not have dependencies in the same table?
        all_ids <- tasks[["task_id"]]

        for (i in all_ids) {
            rowi <- tasks[tasks$task_id == i, ]
            dependencies <-
                strsplit(rowi$deps, split = ",") %>%
                unlist %>%
                unique

            if (any(dependencies %in% all_ids)) {
                next
            }

            run_vars <- c(run_vars, i)
        }

        # run these in parallel:
        subtasks %<>% dplyr::filter(task_id %in% run_vars)
        subtasks_parallel <-
            subtasks %>%
            dplyr::filter(is.na(ncores) | ncores == 1) %>%
            dplyr::arrange(task_id)
        subtasks_no_parallel <-
            subtasks %>%
            dplyr::filter(!is.na(ncores) & ncores != 1) %>%
            dplyr::arrange(task_id)
        
        
        if (nrow(subtasks_parallel) > 0) {
            info("Parallel run tasks:")
            print(subtasks_parallel)
            parallel::mclapply(1:nrow(subtasks_parallel), function (i) {
                w <- subtasks_parallel[i, ]
                delete_logs(w$task_id)
                run_task(id = w$task_id,
                        module_name = w$module_name,
                        question_name = w$question_name,
                        cmd = w$cmd,
                        script = w$script,
                        docopt = w$docopt,
                        export = w$export)
            }, mc.preschedule = F, mc.cores = n_cores) %>% invisible
        }

        

        # These scripts have parallelization within the script
        if (nrow(subtasks_no_parallel) > 0) {
            info("Sequentially run tasks:")
            print(subtasks_no_parallel)
            for (i in 1:nrow(subtasks_no_parallel)) {
                w <- subtasks_no_parallel[i, ]
                delete_logs(w$task_id)
                run_task(id = w$task_id,
                        module_name = w$module_name,
                        question_name = w$question_name,
                        cmd = w$cmd,
                        script = w$script,
                        docopt = w$docopt,
                        export = w$export)
            }
        }

        # If individual tasks did not complete then remove dependent tasks 
        # for next iterations
        bad_tasks <- 
            dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>%
            dplyr::filter(task_id %in% run_vars) %>%
            dplyr::filter(status != "done") %>%
            dplyr::select(task_id) %>%
            dplyr::collect(n = Inf) 
            

        if (nrow(bad_tasks) > 0) {
            # Print information about failed tasks
            info("Failed or hpc-waiting tasks: " %^% bad_tasks$task_id)

            # Any tasks in tasks dependent on this?        
            remove_ids <- 
                lapply(bad_tasks$task_id, function(i) {
                   find_deps_reverse_rec(i, tasks)
                }) %>% unlist %>% unique
            if (length(remove_ids) > 0) {
                info("Removing these tasks because dependency did not finish:" %^% remove_ids)
                for (i in remove_ids) {
                    update_task_status_no_timestamp(
                        status = "waiting",
                        id = i,
                        db = db)
                }
            }
        } else {remove_ids <- NULL}


        # Remove tasks that were run
        tasks %<>% dplyr::filter(!task_id %in% c(run_vars, remove_ids))

        if (nrow(tasks) == 0) 
            break
        counter <- counter + 1
    }


}

#' @export
delete_logs <- function(id, db_log = pg_connect(Sys.getenv("DS_VERSION"))) {
    DBI::dbGetQuery(db_log,
        "DELETE FROM pipe.logs WHERE task_id=" %^% id %^% ";") %>% invisible
    DBI::dbDisconnect(db_log) %>% invisible
}

#' @export
delete_coder_question <- function(question_id, db) {
    DBI::dbGetQuery(db,
        "DELETE FROM pipe.coder_table WHERE question_id=" %^% question_id %^% ";")
}

#' @export
get_ncores <- function(id, db) {
    mod_id <-
        dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>%
        dplyr::filter(task_id == as.numeric(id)) %>%
        dplyr::collect(n = Inf) %$%
        module_id
    ncpus <-
        dplyr::tbl(db, dbplyr::in_schema("pipe", "modules")) %>%
        dplyr::filter(module_id == mod_id) %>%
        dplyr::collect(n = Inf) %$%
        ncores
    ncpus
}


#' @export
find_deps_rec <- function(idx, make_tbl) {
    if (is.null(idx)) return(NULL)
    d <-
        make_tbl %>%
        dplyr::filter(task_id == idx) %$%
        deps %>%
        strsplit(., ",") %>%
        unlist
    if (length(d) == 1) if(unique(d) == 0) return(0)
    c(d, lapply(d, function(a) {find_deps_rec(a, make_tbl)}) %>% unlist)
}

#' @export
trace_deps <- function(ids, make_tbl) {
    # keep only ids and dependencies
    keep <- ids
    for (id in ids) {
        deps <- find_deps_rec(id, make_tbl)
        keep <- c(keep, deps)
    }
    keep <- unique(keep)
    make_tbl %>% dplyr::filter(task_id %in% keep)
}

#' @export
trace_reverse_deps <- function(ids, make_tbl) {
    # keep only ids and dependencies
    keep <- ids
    for (id in ids) {
        deps <- find_deps_reverse_rec(id, make_tbl)
        keep <- c(keep, deps)
    }
    keep <- unique(keep)
    make_tbl %>%
        dplyr::filter(task_id %in% keep) %>%
        dplyr::arrange(task_id)
}

#' @export
find_deps_reverse_rec <- function(idx, make_tbl) {
    if (is.null(idx)) return(NULL)
    bool <-
        grepl("^" %^% idx %^% "$", make_tbl$deps) |
        grepl("^" %^% idx %^% "[,]", make_tbl$deps) |
        grepl("[,]" %^% idx %^% "[,]", make_tbl$deps) |
        grepl("[,]" %^% idx %^% "$", make_tbl$deps)
    if (sum(bool) == 0) return(NULL)
    d <-
        make_tbl %>%
        dplyr::filter(bool) %$% task_id
    c(d, lapply(d, function(a) {find_deps_reverse_rec(a, make_tbl)}) %>% unlist)
}



#' @export
check_timestamps <- function(db) {
    
    anyChange <- TRUE
    nn <- 1
    while (anyChange) {
        info(nn)
        loop_table <- DBI::dbGetQuery(db, "select * from pipe.make;") %>%
            dplyr::filter(!is.na(ts))

        dep_table <- loop_table %>%
            tidyr::unnest(deps = strsplit(deps, ",")) %>%
            dplyr::select(task_id, deps, task_ts = ts, task_status = status) %>%
            dplyr::mutate(deps = as.numeric(deps)) %>%
            dplyr::left_join(select(loop_table, deps = task_id, dep_ts = ts, dep_status = status),
                by = "deps") %>%
            dplyr::filter(deps != 0) %>%
            dplyr::rename(dep_id = deps)

        bool <- (dep_table$task_ts < dep_table$dep_ts | 
            (dep_table$dep_status %in% c("recalc", "waiting"))) &
                (!dep_table$task_status %in% c("recalc", "waiting"))
        if (anyNA(bool)) {
            info("Some tasks do not track to dependency 0!")
            dep_table %>% 
                dplyr::filter(is.na(bool)) %>% 
                dplyr::select(task_id) %>% 
                print(.)
        }
        

        if (any(bool)) {
            anyChange <- TRUE
        } else {
             anyChange <- FALSE
             info("done with checks!")
        }

        mismatch_id <- 
            dep_table$task_id[bool] %>% 
            unique

        if (length(mismatch_id) > 0) {
            for (i in 1:length(mismatch_id)) {
                print("Timestamp mismatch detected for TASK_ID: " %^% mismatch_id[i])
                    update_task_status_no_timestamp(status = "recalc",
                        id = mismatch_id[i],
                        db = db)
            }
        } else {
            info("All timestamps are now fine.")
        }

        nn <- nn + 1
    }
}

#' @export
find_latest_dep <- function(name, tasks) {

    # find latest step that is still connected to deps = 0.
    name_tasks <- tasks %>% dplyr::filter(question_name == name)
    if (nrow(name_tasks) == 0)
        stop("There is no task for this question!: " %^% name)

    if (nrow(name_tasks) == 1)
        return(name_tasks$task_id)

    all_deps <-
        trace_deps(name_tasks$task_id, tasks)$deps %>%
        strsplit(split = ",") %>% unlist %>% as.numeric %>% unique

    # find a task in name_tasks that is connected to 0
    # and no other task depends on it!
    last_id <- numeric(0)
    for (i in 1:nrow(name_tasks)) {
        deps <- trace_deps(name_tasks$task_id[i], tasks)
        deps <- deps[deps$task_id != name_tasks$task_id[i], ]
        # if (!0 %in% deps$deps)
        #    next

        # No tasks depends on this one:
        if (!name_tasks$task_id[i] %in% all_deps) {
            last_id <- c(last_id, name_tasks$task_id[i])
        }
    }

    # If we catch more than one take the larger task_id
    if (length(last_id) > 1)
        last_id <- max(last_id)

    if (length(last_id) == 0) {
        info("Did not find latest dependency for " %^% name)
        return(NA_real_)
    }
    last_id
}


#' @export
find_latest_dep_vec <- function(many_names, tasks) {
    lapply(many_names, function(na) {
        find_latest_dep(na, tasks)
        }) %>% unlist
}

#' Get the task_id for a new task
#'
#' @param var_name A character vector; it can be of any length.
#'
#' @param conn A DBI connection to PostgreSQL database that has a schema called "pipe".
#'
#' @export
make_id <- function(var_name, conn) {
    prev_id <- dplyr::tbl(conn, dbplyr::in_schema("pipe", "make")) %>%
        dplyr::summarise(task_id_max = max(task_id, na.rm = TRUE)) %>%
        dplyr::pull(task_id_max)
    id <- seq({prev_id + 1}, {prev_id + length(var_name)}, 1)
    return(id)
}

#' Create modules for Make table in PostgreSQL
#'
#' @param mod_name A character vector of module names that need to be added for a variable.
#'
#' @param var_name A character vector of length 1 that specifies the variable for which the module is added.
#'
#' @param make_df A data.frame with all tasks.
#'
#' @param conn A PostgreSQL connection.
#'
#'
#' @export
make_module <- function(mod_name, var_name, make_df, conn) {

    mod_id <- dplyr::tbl(conn, dbplyr::in_schema("pipe", "modules")) %>%
        dplyr::filter(module_name %in% mod_name) %>%
        dplyr::collect %>%
        .[match(mod_name, .[["module_name"]]),] %>%
        dplyr::pull(module_id)

    deps <- sapply(1:length(mod_name), function(count) {
        if(mod_name[count] == "download_var") {
            as.character(0)
        } else if(mod_name[count] == mod_name[1] && !any(mod_name %in% "download_var")) {
            as.character(find_latest_dep(var_name, make_df))
        } else {
            as.character(id[count] - 1)
        }
    })

    df_up <- data.frame(
        module_id = mod_id,
        module_name = mod_name,
        question_name = var_name,
        deps = deps,
        status = "not_started",
        lock = FALSE,
        stringsAsFactors = FALSE
        )

    return(df_up)
}

#' @export
check_direct_deps <- function(id = Sys.getenv("TASK_ID"), db) {

    df <-
        dplyr::tbl(db, dbplyr::in_schema("pipe", "make")) %>%
        dplyr::collect(n = Inf) %>%
        as.data.frame(stringsAsFactors = F)
    dependencies <- strsplit(df$deps[df$task_id == id], split = ",") %>%
        unlist %>%
        as.integer
    df %<>%
        dplyr::filter(task_id %in% dependencies) %>%
        dplyr::select(task_id, question_name, status, module_name)
    # stop if any dependencies are not done yet
    if (any(df$status != "done")) {
        update_task_status_no_timestamp(status = "waiting", db = db)
        info("Some dependencies are not done yet!")
        quit(save = "no", status = 0)
    }
    info("All deps are fine...")
}

