options(warn = 2)

suppressMessages(library(vutils))
suppressMessages(library(dplyr))
set_env(MODULE_NAME = "elffelrbin_cleaning")

# Functions
# --------------------------------------------------------------------------
main <- function(objs, country, utable, cleaning_var, VARNAME) {
    out <- list()
    out[[VARNAME]] <- objs[[VARNAME]][[VARNAME]]
    out[[VARNAME]]$cd <- NULL
    out[[VARNAME]]$cy <- NULL
    lapply(c("cd", "cy"), function(lev) {
        date_col <- if(lev == "cd") "historical_date" else "year"
        drop_col <- if(date_col == "historical_date") "year" else "historical_date"

        df <- objs[[VARNAME]][[VARNAME]][[lev]] %>% add_country_cols(country)
        n <- names(df)
        df_clean <- objs[[cleaning_var]][[cleaning_var]][[lev]] %>% add_country_cols(country)

        df_clean[[drop_col]] <- NULL
        df[[drop_col]] <- NULL

        # Adjust to use _ord, because v2elffelrbin goes through the mm
        if (VARNAME == "v2elffelr")
            cleaning_var <- "v2elffelrbin_ord"

        # 
        full_df <-
            full_join(df, df_clean,
                      by = c("country_id", "country_text_id", date_col)) %>%
            add_date_cols %>%
            interpolate_vdem_style(cleaning_var, utable) %>%
            filter(!(is.na(fill_col) | fill_col == 0)) %>%
            select(one_of(n), -country_text_id)
        full_df <- full_df[!is.na(full_df[[VARNAME]]),] # is this correct?
    out[[VARNAME]][[lev]] <<- full_df
  }) %>% invisible
  return(out)
}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
  # Global variables
  get_globals()

  # Imports
  country <- load_country()
  utable <- load_country_unit()

  objs <- find_dep_files(TASK_ID, DB)
  cleaning_var <- names(objs)[names(objs) != VARNAME]

  # Run
  collectedInput <- named_list(objs, country, utable, cleaning_var, VARNAME)
  do.call(main, collectedInput) %>%
    write_file(., OUTFILE, dir_create = TRUE)

  } else {
    testthat::test_file("~/proj/mm-prep/tests/init/test_post_mm_cleaning.R")
  }
update_task_status(db = DB)
