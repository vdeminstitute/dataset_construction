#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()

# Load dependencies
objs <- find_dep_files(TASK_ID, db)
country <- load_country()

# Load each dependency and merge
df <-
    lapply(names(objs), function(v) {
            objs[[v]][[v]]$cy %>% add_country_cols(country) %>%
            add_date_cols
    }) %>%
    Reduce(full_join_vdem, .)

df %<>% arrange(country_id, historical_date)

write_file(df, file.path(ROOT, "stata/structure_of_executives_input.dta"),
           dir_create = T)



# Run STATA script
# ------------------------------------------------------------------------------
home <- Sys.getenv("HOME")
system("cd " %^% home %^% "/proj/vdemds/stata_indices/ && " %^% "~/.local/bin/stata -b do structure_of_executives.do")
# Let's read the log file and print it so that it goes to the log table.
d <- readLines(home %^% "/proj/vdemds/stata_indices/structure_of_executives.log")
# Remove lines concerning license
d <- d[-(1:21)]
d <- d %>% trimws(which = "both") %>% .[. != ""]
print(shQuote(d))

# Also check log file for error messages and send status error
# STATA will return some error code e.g. r(199) if an error occurs
if (any(grepl("r(", d, fixed = T)) | any(grepl("expired", d))) {
    stop("structure_of_executives failed!")
}

# Read data
df <- read_file(file.path(ROOT, "stata/structure_of_executives_output.dta"),
                convert.factors = F)

# write file
out <- list()
v <- c("v2x_ex_hereditary", "v2x_ex_military", "v2x_ex_party",
       "v2x_ex_direlect", "v2x_ex_confidence")
lapply(v, function(va) {
        out[[va]]$cy <<-
            df %>% add_country_cols(country) %>%
            .[, c("country_text_id", "country_id", "year", va)]
    }) %>% invisible

write_file(out, OUTFILE, dir_create = T)
