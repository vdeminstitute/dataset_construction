#!/usr/bin/env Rscript

# Purpose: 
# -- (1) Create a data.frame of variable labels that are to be used  later for the Stata Version of the dataset.
# -- (2) Clean the codebook table

suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))
suppressMessages(library(DBI))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# -- Get database for pipeline
db <- pg_connect()

# -- Global variables
get_globals()
dbCodebook <- read_file(file.path(ROOT, "download", "codebook.rds"))

# -- CB_RELEASE will be matched against datarelease from codebook table
CB_RELEASE <- Sys.getenv("CB_RELEASE")
info(sprintf("Generating codebook table with CB_RELEASE set to %s", CB_RELEASE))

# Generate variable labels
# The ordinal versions ("_3, _4, _5") variables do not have separate entries in the codebook table.
# They are needed in order to generate question labels for the Stata dataset.
# As an example, e_v2x_polyarchy_3C, 4C, 5C has one entry but we 
# want one entry per category. Hence, we break them into their reduced form.
# The labels for all variables are saved in refs/question_labels.rds.

# From codebook table we remove parts of a tag that contains:
# 1: \ and
# 2: anything that follows after a ","
qlabels <-
    select(dbCodebook, question_id, label = name, name = tag) %>%
    mutate(name = sub(",.*$", "", gsub("\\", "", name, fixed = TRUE)))

# Reduce the ordinalized tags (i.e. one entry per category).
idx <- grep("_3C", qlabels$name)
# -- From the 3C name, create 4C and 5C names
hli_ord_names <-
    sub("_3C.*$", "", qlabels$name[idx]) %>%
    lapply(paste0, c("_3C", "_4C", "_5C")) %>%
    unlist()
# -- Expand the the stem name to one per category
hli_ord_labels <- qlabels[rep(idx, each = 3), ]
stopifnot(length(hli_ord_names) == nrow(hli_ord_labels))

# -- Add new hli_ordinal labels and write to refs
hli_ord_labels %>%
    mutate(name = hli_ord_names) %>%
    bind_rows(qlabels[-idx, ]) %>%
    write_file(file.path(ROOT, "refs", "question_labels.rds"), dir_create = TRUE)

# Generate codebook table
# We recode every character variable to ensure correct encoding.
# We only save variables that are to be in the dataset, per the CB_RELEASE filtering.
codebook <-
    mutate_if(dbCodebook, is.character, function(v) {
        # This is just for Windows
        Encoding(v) <- "UTF-8"

        # The uploaded text has a weird mix of html and
        # latex. Strip the former and replace with the latter
        # where appropriate.
        gsub("<br>", "\\\\ \n", v, fixed = TRUE) %>%
            gsub("~", "\"", .) %>%
            gsub("precoded", "pre-coded", .)
    }) %>%
    mutate(tag = gsub("\\", "", tag, fixed = TRUE)) %>% 
    # datarelease is matched against CB_RELEASE
    filter(grepl(CB_RELEASE, datarelease))

# Write codebook table to refs
write_file(codebook, file.path(ROOT, "refs", "codebook.rds"))
