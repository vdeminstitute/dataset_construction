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
codebook <-
    mutate_if(dbCodebook, is.character, function(v) {
        Encoding(v) <- "UTF-8"

        gsub("<br>", "\\\\ \n", v, fixed = TRUE) %>%
            gsub("~", "\"", .) %>%
            gsub("precoded", "pre-coded", .)
    }) %>%
    mutate(tag = gsub("\\", "", tag, fixed = TRUE)) %>% 
    filter(grepl(CB_RELEASE, datarelease))

# Write codebook table to refs
write_file(codebook, file.path(ROOT, "refs", "codebook.rds"))