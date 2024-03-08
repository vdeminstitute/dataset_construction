#!/usr/bin/env Rscript
#
# Purpose:
# Match vignettes to the associated question_ids.
# Match historical and contemporary vignettes to the same question_id
# 	where reasonable.
# Merge vignettes for male/female versions of certain variables.
# 
# As a result we want a vignette_table that contains one row per
# vignette.
# 
# Vignettes are questions asked to the coders in order to better
# distinguish coder-specific thresholds, vignettes are questions asked about
# hypothetical countries, but with the same questions and answer categories
# as our usual survey questions. Vignettes are designed to capture coder 
# thresholds from one answer category to another, i.e. the threshold between
# e.g. answer category 3 and 4.
#
# The vignette_parent table in postgres tells us which vignettes 
# belong to which parent question_id.
# Vignettes have question_ids and entries in the question table
# just like regular questions in our surveys.
# The vignette_paris table in postgres tells us which historical
# vignettes are merged with related contemporary vignettes.

suppressMessages(library(dplyr))
suppressMessages(library(DBI))
suppressMessages(library(vutils))
suppressMessages(library(vbase))
suppressMessages(library(vpipe))

# -- Functions
find_hist_vignettes <- function(variable, qtable) {
    v <- gsub("v2|v3", "", variable)
    qtable %>%
        filter(grepl(v, name)) %>%
        filter(grepl("^B_", name)) %>%
        select(vignette_id = question_id, vignette_name = name) %>%
        mutate(top_name = v) %>%
        mutate(threshold = sub("^.*(\\d)_?\\S?$", "\\1", vignette_name) %>%
                               as.numeric)
}

find_all_vignettes <- function(variable, qtable) {
    v <- gsub("v2|v3", "", variable)
    qtable %>%
        filter(grepl(v, name)) %>%
        filter(grepl("^[AB]", name)) %>%
        select(vignette_id = question_id, vignette_name = name) %>%
        mutate(top_name = v)
}

# -- Get database for pipeline
db <- pg_connect()
# -- Global variables
get_globals()

# -- Read tables
qtable <- load_qtable()

# vignette pairs between contemporary and historical projects
# -- used for merging
info("Observe that we use a static table ['vignette_pairs'] to decide which v2 and v3 vignettes to merge.")
vignette_pairs <- distinct(
    read_file(file.path(ROOT, "download", "vignette_pairs.rds")),
    vignette_name, merge_with_cont)
stopifnot(no_duplicates(vignette_pairs, cols = c("vignette_name", "merge_with_cont")))
info(sprintf("vignette_pairs contains %d vignette pairs.", nrow(vignette_pairs)))

# merge vignette_pairs with qtable
# -- hist_merged_id is question_id from qtable
# -- merge_with_cont is the contemporary name of the vignette
# -- vignette_name is the historical name of the vignette
stopifnot(all(vignette_pairs$merge_with_cont %in% qtable$name))
vignette_pairs <-
    merge(
        x = vignette_pairs,
        y = qtable[, c("question_id", "name")],
        by.x = "merge_with_cont", by.y = "name") |> 
    dplyr::rename(hist_merged_id = question_id)
stopifnot(no_duplicates(vignette_pairs, cols = c("vignette_name", "hist_merged_id")))
info(sprintf("After merging vignette_pairs with qtable, we have %d vignette pairs.",
    nrow(vignette_pairs)))

# Vignettes table
# -- vignette is matched to its "parent" via parent_question_id and vignette_id
info(sprintf("We use a static table ['vignette_parent'] to match vignettes to their parent."))
vignette_parent <-
    read_file(file.path(ROOT, "download", "vignette_parent.rds")) |> 
    dplyr::rename(vignette_id = vignette_question_id)
info(sprintf("vignette_parent contains %d vignettes.", nrow(vignette_parent)))

# Create vtable with all mm-variables
# -- parent_name is the v2 version of a variable
# -- hist_merged is TRUE if the variable is merged
vtable <-
    subset(qtable, mm) |> 
    subset(!grepl("_\\d+$", name)) |> 
    subset(!grepl("_rec$", name)) |> 
    subset(select = c(question_id, name, hist_merged))
names(vtable) <- gsub("name", "parent_name", fixed = TRUE, x = names(vtable))
info(sprintf("vtable created from qtable contains %d variables.", nrow(vtable)))

# -- Contemporary vignettes (and for some reason v3elmalalc)
vtableVignetteParent <- merge(
        x = vtable,
        y = vignette_parent,
        by.x = "question_id",
        by.y = "parent_question_id",
        all.x = TRUE)
stopifnot(all(vtable$question_id %in% vtableVignetteParent$question_id))

# -- vignette table for v2 vignettes
vtable_cont <- merge(
    x = vtableVignetteParent,
    y = qtable[, c("question_id", "name")],
    by.x = "vignette_id",
    by.y = "question_id",
    all.x = TRUE)
stopifnot(nrow(vtable_cont) == nrow(vtableVignetteParent))

vtable_cont <- vtable_cont %>% 
    dplyr::rename(vignette_name = name) |> 
    mutate(top_name = gsub("v2|v3", "", parent_name)) |> 
    mutate(threshold = as.numeric(sub("^.*_\\d(\\d)_\\d{2}$", "\\1", vignette_name))) |> 
    arrange(parent_name, threshold) |> 
    filter(!is.na(vignette_id))

# These v2 variables are merged between contemporary and historical
merge_vars <-
    filter(vtable, hist_merged, grepl("^v2", parent_name)) |> 
    getElement("parent_name") |> 
    unique() |> 
    sort()
stopifnot(!grepl("^v3", merge_vars))

vtable_merge <-
    lapply(merge_vars, function(v) {
        find_hist_vignettes(v, qtable)}) %>%
    bind_rows() %>%
    left_join(qtable %>%
                  filter(grepl("^v2", name)) %>%
                  select(question_id, parent_name = name, hist_merged) %>%
                  mutate(top_name = gsub("v2", "", parent_name)),
              by = "top_name") %>%
    select(question_id, parent_name, hist_merged, vignette_id, vignette_name,
           top_name, threshold) %>%
    arrange(parent_name, threshold)

# These variables are historical
hist_vars <-
    vtable |> 
    filter(grepl("^v3", parent_name)) |> 
    getElement("parent_name") |> 
    unique() |>
    sort()

vtable_hist <-
    lapply(hist_vars, function(v) {find_hist_vignettes(v, qtable)}) %>%
    bind_rows %>%
    left_join(qtable %>%
              filter(grepl("^v3", name)) %>%
              select(question_id, parent_name = name, hist_merged) %>%
              mutate(top_name = gsub("v3", "", parent_name)),
          by = "top_name") %>%
    select(question_id, parent_name, hist_merged, vignette_id, vignette_name,
           top_name, threshold) %>%
    arrange(parent_name, threshold)

# vtable for contemporary and historical vignettes
vtable_merged <-
    bind_rows(vtable_cont, vtable_merge, vtable_hist) |> 
    arrange(parent_name, threshold)

# Match male / female vignettes
# -- as we separate male and female mm variables, we also need to separate them
# in the vignette table
femvig <-
    vtable_merged %>%
    filter(parent_name %in% c("v2clacjstm", "v2cldiscm",
                              "v2clprptym", "v2clslavem",
                              "v2juhcind")) %>%
    mutate(parent_name = case_when(
        parent_name == "v2clacjstm" ~ "v2clacjstw",
        parent_name == "v2cldiscm" ~ "v2cldiscw",
        parent_name == "v2clprptym" ~ "v2clprptyw",
        parent_name == "v2clslavem" ~ "v2clslavef",
        parent_name == "v2juhcind" ~ "v2juncind",
        TRUE ~ parent_name)) %>%
    mutate(question_id = to_qids(parent_name, qtable))

vtable_merged <- bind_rows(vtable_merged, femvig)

# These variables do not have any matching vignettes
missing_vignettes <- filter(vtable, !question_id %in% vtable_merged$question_id)
info("The following variables lack matching vignettes:")
print(organiseRows(missing_vignettes, question_id))

if (nrow(missing_vignettes) > 0) {
    # Attempt to find missing vignettes
    found_missing_vignettes <-
        filter(vtable, !question_id %in% vtable_merged$question_id) %$%
        lapply(parent_name, function(v) {
            find_all_vignettes(v, qtable)}) %>% 
        bind_rows()

    if (nrow(found_missing_vignettes) == 0) {
        info("None of the missing vignettes could be found.")
    } else {
        info("The following vignettes were found for the missing variables:")
        print(organiseRows(found_missing_vignettes, question_id))
        stop("Missing vignettes were found.", call. = FALSE)
    }
}

vtable_merged <-
	select(vtable_merged, -top_name) |> 
    mutate(type = ifelse(grepl("^A", vignette_name),
        "contemporary", "historical")) |> 
    group_by(question_id, type, threshold) |> 
    arrange(question_id, type, threshold, vignette_name) |> 
    # -- hardcoded fix for a vignette
	mutate(threshold = case_when(
		vignette_name == "B_me_v2mecenefm_01_01" ~ 1,
		vignette_name == "B_me_v2mecenefm_12_01" ~ 2,
		vignette_name == "B_me_v2mecenefm_23_01" ~ 3,
		vignette_name == "B_me_v2mecenefm_34_01" ~ 4,
		TRUE ~ threshold)) |> 
    mutate(id = vignette_name %^% "__" %^% threshold) |> 
    ungroup() |> 
    as.data.frame()

# Last checks
stopifnot(vtable_merged$threshold[
	vtable_merged$vignette_name == "B_me_v2mecenefm_34_01"] == 4)
stopifnot(no_duplicates(vtable_merged, cols = c("question_id", "vignette_id")))

# Append merging information from vignette_pairs
vtable_res <- merge(
    x = vtable_merged,
    y = vignette_pairs,
    by = "vignette_name",
    all.x = TRUE)
stopifnot(nrow(vtable_res) == nrow(vtable_merged))

write_file(vtable_res, file.path(ROOT, "refs", "vignette_table.rds"))