#!/usr/bin/env Rscript
#
# Creates the final public DS with proper variable labels.
#
# Input is the dataset directory. By this point we expect base file
# for CD, CY, and CY+Others.
#
# Note: for character encoding purposes we save the Stata files in
# version 14 (118) format --- first Stata version that switched to
# UTF-8. They need to be converted to version 13, and value labels
# applied, using the `convert.do` script.
###

# options(warn = 2)

suppressMessages(library(dplyr))
suppressMessages(library(dbplyr))
suppressMessages(library(magrittr))
suppressMessages(library(tidyr))
suppressMessages(library(vutils))
suppressMessages(library(parallel))
suppressMessages(library(vanalysis))

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
ROOTDIR <- ROOT
DIR       <- file.path(ROOTDIR, "dataset")
STATA_DIR <- file.path(ROOTDIR, "dataset_stata")
REFS      <- file.path(ROOTDIR, "refs")
VERSION <- Sys.getenv("DS_VERSION")
tasks <- tbl(db, in_schema("pipe", "make")) %>% collect(n = Inf)
deps <- tasks %>% filter(question_name == "dataset") %$% deps %>%
    strsplit(",") %>% unlist %>% as.numeric
stopifnot(length(deps) == length(unique(deps)))


# Load and adjust references
#-------------------------------------------------------------------------------
qtable <-
    read_file(file.path(REFS, "question_table.rds")) %>%
    filter(!(conthistmerge == "merge" & grepl("^v3", name))) %>%
    filter(!grepl("approval", datarelease))
qtable %<>% filter(!grepl("_covid$", name))


codebook <-
    read_file(file.path(REFS, "codebook.rds")) %>%
    filter(grepl(Sys.getenv("CB_RELEASE"), datarelease)) %>%
    mutate(tag = sub(",.*", "", tag) %>% # Grab the root variable name from tag
               gsub("\\", "", ., fixed = T)) %>%
    select(question_id, tag, question_number, cb_section, sources)

utable <- read_file(file.path(REFS, "country_unit.rds"))
country   <- read_file(file.path(ROOT, "refs", "country_table.rds"))
ctable <- country

vdem <- c("id", "x1", "x2",
          "elGeneral", "elSuffrage", "elQualities", "elOutcomes",
          "elPresidential", "elLower", "elSubnational",
          "ps",
          "ddInitiatives", "ddReferendums", 
          "ddObligatory",
          "ddPlebiscite", "ddOccurence",
          "exHOS", "exHOG", "exExecutive", "exRegime",
          "lgGeneral", "lgUpper", "lgLower",
          "dl", "ju",
          "clPersonal", "clImpartial", "clPrivate", "clProperty",
          "sv", "cs", "me", "pe",
          "exlSocgroup", "exlGender", "exlUrban", "exlPolitical", "exlSocial",
          "leg",
          "ca_pk", "ca_mob", "ca_eng", "ca_ac",
          "histel", "histps", "histex", "histlg", "histju", "histcl", "histst",
          "histpe", "hist", "histover")
x3_sections <-
    c("x3regime", "x3account", "x3chiefex", "x3neopat", "x3civlib",
        "x3excl", "x3corr", "x3womemp", "x3rol", "x3dd", "x3cs",
        "x3elec", "x3partyinst", "x3condemdim", "x3acfree")
sm_sections <- c("wsmcio", "wsmdmf", "wsmstatereg", "wsmomp", "wsmsc")
e_sections <- c("e1", "e5", "e6", "e7", "e9", "e12", "e13", "e14", "e15",
                "eb1", "eb2", "region", "eb3", "eb5", "eb6", "eb7", "eb8")
cb <- c(vdem, x3_sections, sm_sections, e_sections)

stopifnot(cb %in% codebook$cb_section)

qtable %<>%
    left_join_(data.frame(cb = cb,
                          cb_order = seq_along(cb),
                          stringsAsFactors = F),
               by = c("cb_section" = "cb"))


qtable %<>%
    left_join_(codebook[, c("question_id", "question_number")],
               by = "question_id")

qtable$name[qtable$name == "v2histname"] <- "histname"


# Set variable labels
#-------------------------------------------------------------------------------

qlabels <- qtable %>%
    filter(is.na(class) | class != "V") %>%
    select(question_id, name, codebook_name) %>%
    arrange(name) %>%
    # Remove backslash from labels
    mutate(codebook_name = gsub("\\", "", codebook_name, fixed = T)) %>%
    # adjust multiple selection labels
    mutate(codebook_name = ifelse(grepl("_\\d+$", name),
                          codebook_name %^% gsub("^(.*?_)(\\d+)$", " \\2", name),
                          codebook_name)) %>%
    mutate(codebook_name = ifelse(name == "v2stfisccap",
                          "State fiscal source of revenue", codebook_name)) %>%
    rename(label = codebook_name)

qlabels %<>% mutate(label = ifelse(name == "e_uds_median", 
	"Unified democracy score posterior",
	label))

qtable %<>% filter(name != "histname")

identifiers <- c('country_name','country_text_id','country_id','year',
    'historical_date','project','historical','histname','codingstart',
    'codingend','codingstart_contemp','codingend_contemp','codingstart_hist',
    'codingend_hist','gapstart1','gapstart2','gapstart3','gapend1','gapend2',
    'gapend3','gap_index', 'COWcode')


# load cd files
#-------------------------------------------------------------------------------
fu <- function(v) {

    out <- list()
    # read file
    tryCatch({
        ll <- read_file(find_dep_file_by_task(v))
    }, error = function(err) {
        print("failed to load task: " %^% v)
        return(NULL)
    })

    
    # extract cd and cy
    lapply(c("cd", "cy"), function(lev) {
        tryCatch({
            if (is.data.frame(ll[[1]])) {
                df <- ll[[1]]
            } else {
                df <- ll[[1]][[lev]]
            }

            df <- df %>%
                add_country_cols(country) %>%
                add_date_cols %>%
                select(-country_text_id)

            if (lev == "cd") {
                df %<>% select(-year)
                stopifnot(`There are duplicatesin cd!` = no_duplicates(df,
                    cols = c("country_id", "historical_date")))

            } else if (lev == "cy") {
                df %<>% select(-historical_date)
                stopifnot(`There are duplicatesin cy!` = no_duplicates(df, 
                    cols = c("country_id", "year")))
            }
            out[[lev]] <<- df
            
        }, error = function(err) {
            print("failed to load task: " %^% v)
            return(NULL)
        }, warning = function(war) {
            print("failed to load task: " %^% v)
            return(NULL)
        })
    })
    out
}




# list of cd and cy objects
llf <- mclapply(deps, fu, mc.cores = 6, mc.preschedule = FALSE)
# extract cd data.frames:
cd_files <- lapply(llf, function(ll) {return(ll[["cd"]])})
# extract cy data.frames:
cy_files <- lapply(llf, function(ll) {return(ll[["cy"]])})


# Remove NAs
bool <- lapply(cd_files, is.null) %>% unlist
bad_deps <- deps[bool]
# print missing cases:
tasks %>%
    filter(task_id %in% bad_deps) %>%
    select(question_name, module_name, status) %>%
    as.data.frame(stringsAsFactors = F) %>%
    print
cd_files <- cd_files[!bool]

df_cd <- full_join_vdem_tree(cd_files)


# load cy files
#-------------------------------------------------------------------------------

# Remove NAs
bool <- lapply(cy_files, is.null) %>% unlist
bad_deps <- deps[bool]
# Print missing cases:
tasks %>%
    filter(task_id %in% bad_deps) %>%
    select(question_name, module_name, status) %>%
    as.data.frame(stringsAsFactors = F) %>%
    print

cy_files <- cy_files[!bool]
df_cy <- full_join_vdem_tree(cy_files)

write_file(llf, file.path(ROOTDIR, "llf", "llf.rds"), dir_create = TRUE)
write_file(df_cy, file.path(ROOTDIR, "llf", "df_cy.rds"), dir_create = TRUE)
write_file(df_cd, file.path(ROOTDIR, "llf", "df_cd.rds"), dir_create = TRUE)



rm(llf)
rm(cy_files)
rm(cd_files)


# Clean loaded files
#-------------------------------------------------------------------------------

# 
clean_process <- function(df) {
    print(names(df)[duplicated(names(df))])
    print(grep(".", names(df), fixed = TRUE, value = TRUE))
    df %<>% select(-matches("95"))
    names(df) <- gsub("68", "", names(df))
    df %>% select(-matches("_ord_sd$"))
}

df_cy %<>% clean_process
df_cd %<>% clean_process

df_cy %<>% add_country_cols(country)
df_cd %<>% add_country_cols(country) %>% add_date_cols

stopifnot(no_duplicates(df_cy, c("country_id", "year")))
stopifnot(length(names(df_cy)[duplicated(names(df_cy))]) == 0)
stopifnot(no_duplicates(df_cd, c("country_id", "historical_date")))
stopifnot(length(names(df_cd)[duplicated(names(df_cd))]) == 0)

# sort tables
df_cy %<>% arrange(country_id, year)
df_cd %<>% arrange(country_id, historical_date)

#
# Check if the units fit cu table
# --------------------------------------------------------------------------
# Double-Check last utable cleaning
df_cy <- df_cy %>%
    semi_join(utable, by = c("country_id", "year"))

df_cd <- df_cd %>%
    semi_join(utable, by = c("country_id", "year"))

bad_vars <- qtable %>% 
	filter(class == "C", grepl("^v2reg", name),
	       !to_dichotomize) %$% name
stopifnot(grepl("^v2reg", bad_vars))
stopifnot(isTRUE(!anyNA(df_cd$year)))
stopifnot(isTRUE(!anyNA(df_cy$year)))
lapply(bad_vars, function(v) {
	info("Cleaning: " %^% v)
	vars <- find_vars(v, qtable)
	df_cd[df_cd$year == 2021, vars] <<- NA
	df_cy[df_cy$year == 2021, vars] <<- NA
	return(NULL)
}) %>% invisible
stopifnot(is.na(df_cd[df_cd$year == 2021, c("v2regoppgroupsact_0", "v2regoppgroupsact_nr")]))
stopifnot(is.na(df_cy[df_cy$year == 2021, c("v2regoppgroupsact_0", "v2regoppgroupsact_nr")]))
stopifnot(any(!is.na(df_cd[df_cd$year == 2021, "v2clacfree_osp"])))
stopifnot(any(!is.na(df_cy[df_cy$year == 2021, "v2clacfree_osp"])))


# Calculate Identifiers
#-------------------------------------------------------------------------------

# Define codingstart/end and merge in the rest of our identifiers
idents_df <-
    df_cy %>%
    select(country_id, year, histname = v2histname) %>%
    left_join(select(utable, country_id, year, project, gap_index = gap_idx),
              by = c("country_id", "year")) %>%
    left_join(select(country, country_id, country_name = name),
              by = "country_id") %>%
    arrange(country_id, year) %>%
    group_by(country_id) %>%
    mutate(codingstart = first(year),
           codingend = last(year),
           historical = max(project %in% c("historical-only", "overlap")),
           codingstart_contemp = min(year[project %in%
               c("contemporary-only", "overlap")] %||% NA),
           codingend_contemp = max(year[project %in%
               c("contemporary-only", "overlap")] %||% NA),
           codingstart_hist = min(year[project %in%
               c("historical-only", "overlap")] %||% NA),
           codingend_hist = max(year[project %in%
               c("historical-only", "overlap")] %||% NA),
           project = case_when(project == "contemporary-only" ~ 0,
                               project == "historical-only" ~ 1,
                               project == "overlap" ~ 2)) %>%
    ungroup


# Calculate gaps. Dear God this is horrible.
gaps_df <- select(utable, country_id, year) %>%
    group_by(country_id) %>%
    arrange(year) %>%
    do({
        x <- which(diff(.$year) > 1)
        if (length(x) > 0)
            data.frame(country_id = first(.$country_id),
                       gapstart = .$year[x] + 1,
                       gapend = .$year[x + 1] - 1,
                       stringsAsFactors = F)
        else
            data.frame()
    }) %>%
    summarise(gapstart1 = gapstart[1], gapstart2 = gapstart[2],
        gapstart3 = gapstart[3], gapend1 = gapend[1], gapend2 = gapend[2],
        gapend3 = gapend[3]) %>%
    ungroup

#
# Merge ID variables
# --------------------------------------------------------------------------
df_cy %<>%
    add_date_cols %>%
    left_join(idents_df, by = c("country_id", "year")) %>%
    left_join(gaps_df, by = c("country_id"))
df_cd %<>%
    add_date_cols %>%
    left_join(idents_df, by = c("country_id", "year")) %>%
    left_join(gaps_df, by = c("country_id"))


# Merge e-data into cy
#-------------------------------------------------------------------------------
e_data <- read_file(file.path(ROOT, "external", "e_data.rds")) %>%
	rename(e_uds_median = e_uds)
stopifnot(no_duplicates(e_data, c("country_id", "year")))
df_cy %<>% left_join(e_data, by = c("country_id", "year"))
df_cd %<>% left_join(select(e_data, country_id, year, COWcode),
                     by = c("country_id", "year"))


# Round values to third digit
#-------------------------------------------------------------------------------
fu <- function(v) {
    if (is.numeric(v) & class(v)[1] != "Date") {
        round(v, 3)
    } else {
        v
    }
}
df_cy[, ] <- lapply(df_cy, fu)
df_cd[, ] <- lapply(df_cd, fu)


# Set NaN to NA
#-------------------------------------------------------------------------------
is.na(df_cy) <- is.na(df_cy)
is.na(df_cd) <- is.na(df_cd)


# Compare datasets
#-------------------------------------------------------------------------------
if (interactive()) {
    ds_old <- read_file("~/data/ds_construction/v11/dataset/V-Dem-CY-Full+Others/V-Dem-CY-Full+Others-v11.1.rds")
    # what is in new but not in old?
    setdiff(names(df_cy), names(ds_old)) %>% sort
    # What is in old but not in new?
    setdiff(names(ds_old), names(df_cy)) %>% sort
    rm(ds_old)
}

is_vdem_sorted <- function(df) {
    identical(df, dplyr::arrange(df, country_id, historical_date))
}


stopifnot(is_vdem_sorted(df_cy))
stopifnot(is_vdem_sorted(df_cd))


###
# Country-Year: Core
# V-Dem indices + components
#-------------------------------------------------------------------------------
info("V-Dem-CY-Core")
dir.create(file.path(DIR, "V-Dem-CY-Core"), recursive = T, showWarnings = F)
dir.create(STATA_DIR, recursive = T, showWarnings = F)

# Select columns
indices <-
    qtable %>%
    filter(grepl("^x\\d+", cb_section)) %$% name

comps <-
    lapply(indices, function(v) rec_components(v, codebook)) %>%
    unlist %>%
    unique

all_vars <- c(indices, comps) %>% unique
sorted_vars <-
    qtable %>%
    filter(name %in% all_vars) %>%
    arrange(cb_order, question_number) %$% name

cy_core_vars <-
    lapply(sorted_vars, function(v) {find_vars(v, qtable)}) %>%
    unlist %>%
    unique %>% 
    sort_multiple_selection

# Report missing columns
missvar <- cy_core_vars[!cy_core_vars %in% colnames(df_cy)]
if (length(missvar) > 1)
    info("Missing variables: " %^% missvar)

df_country_year_core <-
    df_cy %>%
    select(one_of(identifiers), one_of(cy_core_vars))
info(sprintf("CY rows: %d, cols: %d", 
	nrow(df_country_year_core), ncol(df_country_year_core)))

# Set labels
cy_labels <- colnames(df_country_year_core) %>% to_qlabels(qlabels)

# Write dataset files
# rds
fork1 <- mcparallel({
    info("Creating R CY")
    write_file(df_country_year_core,
               file.path(DIR, "V-Dem-CY-Core",
                         "V-Dem-CY-Core-" %^% VERSION %^% ".rds"))
})

# csv
fork2 <- mcparallel({
    info("Creating CSV CY")
    write_file(df_country_year_core,
               file.path(DIR, "V-Dem-CY-Core",
                         "V-Dem-CY-Core-" %^% VERSION %^% ".csv"))
})

# SPSS
fork3 <- mcparallel({
    info("Creating SPSS CY")
    write_file(set_spss_labels(df_country_year_core, cy_labels),
               file.path(DIR, "V-Dem-CY-Core",
                         "V-Dem-CY-Core-" %^% VERSION %^% ".sav"))
})

# STATA
fork4 <- mcparallel({
    info("Creating Stata CY")
    write_file(set_stata_labels(df_country_year_core, cy_labels),
               file.path(STATA_DIR,
                         "V-Dem-CY-Core-" %^% VERSION %^% ".dta"),
               version = 118,
               data.label = "V-Dem-CY-Core")
})



###
# Country-Year: V-Dem Extended (V-Dem + Other Sources)
#-------------------------------------------------------------------------------
info("V-Dem-CY-Full+Others")
dir.create(file.path(DIR, "V-Dem-CY-Full+Others"), showWarnings = F)

# Select variables
sorted_vars <-
    qtable %>%
    filter(cb_section %in% cb) %>%
    arrange(cb_order, question_number) %$% name
cy_extended_vars <-
    lapply(sorted_vars, function(v) {find_vars(v, qtable)}) %>%
    unlist %>%
    unique %>% 
    sort_multiple_selection

cy_others.df <- df_cy

# Grab columns
missvar <- cy_extended_vars[!cy_extended_vars %in% colnames(cy_others.df)]
if (length(missvar) > 1)
    info("Missing variables: " %^% missvar)

cy_others.df %<>% select(one_of(identifiers), one_of(cy_extended_vars))
info(sprintf("CY-Full+Others rows: %d, cols %d",
             nrow(cy_others.df), ncol(cy_others.df)))

# Labels
others_labels <- colnames(cy_others.df) %>% 
	gsub("e_uds_mean", "e_uds_median", ., fixed = TRUE) %>%
	gsub("e_uds_pct025", "e_uds_median", ., fixed = TRUE) %>%
	gsub("e_uds_pct975", "e_uds_median", ., fixed = TRUE) %>%
	gsub("e_gdp_sd", "e_gdp", ., fixed = TRUE) %>%
	gsub("e_gdppc_sd", "e_gdppc", ., fixed = TRUE) %>%
	gsub("e_pop_sd", "e_pop", ., fixed = TRUE) %>%
	to_qlabels(qlabels)

# rds
fork6 <- mcparallel({
    info("R CY Full+Others")
    write_file(cy_others.df,
               file.path(DIR, "V-Dem-CY-Full+Others",
                         "V-Dem-CY-Full+Others-" %^% VERSION %^% ".rds"))
})

# csv
fork7 <- mcparallel({
    info("Creating CSV CY-Full+Others")
    write_file(cy_others.df,
               file.path(DIR, "V-Dem-CY-Full+Others",
                         "V-Dem-CY-Full+Others-" %^% VERSION %^% ".csv"))
})

# SPSS
fork8 <- mcparallel({
    info("Creating SPSS CY-Full+Others")
    write_file(set_spss_labels(cy_others.df, others_labels),
               file.path(DIR, "V-Dem-CY-Full+Others",
                         "V-Dem-CY-Full+Others-" %^% VERSION %^% ".sav"))
})

# STATA
fork9 <- mcparallel({
    info("Creating Stata CY-Full+Others")
    write_file(set_stata_labels(cy_others.df, others_labels),
               file.path(STATA_DIR,
                         "V-Dem-CY-Full+Others-" %^% VERSION %^% ".dta"),
               version = 118,
               data.label = "V-Dem CY-Full+Others")
})





###
# Country-Date: V-Dem, HLIs + all v2 and unique v3 (why only unique v3?)
#-------------------------------------------------------------------------------
info("V-Dem-CD Full")
dir.create(file.path(DIR, "V-Dem-CD"), showWarnings = F)

# Read file
cd.df <- df_cd

# Variable selection
sorted_vars <-
    qtable %>%
    filter(cb_section %in% c(vdem, x3_sections, sm_sections)) %>%
    filter(cb_section != "x3account") %>%
    arrange(cb_order, question_number) %$% name
cd_vars <-
    lapply(sorted_vars, function(v) {find_vars(v, qtable)}) %>%
    unlist %>%
    unique %>% 
    sort_multiple_selection

# Grab columns
missvar <- cd_vars[!cd_vars %in% colnames(cd.df)]
if (length(missvar) > 1)
    info("Missing variables: " %^% missvar)

cd.df %<>% select(one_of(identifiers), one_of(cd_vars))
info(sprintf("CD rows: %d, cols %d", nrow(cd.df), ncol(cd.df)))

# Labels
cd_labels <- colnames(cd.df) %>% to_qlabels(qlabels)

# rds
fork10 <- mcparallel({
    info("Creating R CD")
        write_file(cd.df,
                   file.path(DIR, "V-Dem-CD",
                             "V-Dem-CD-" %^% VERSION %^% ".rds"))
})

# csv
fork11 <- mcparallel({
    info("Creating CSV CD")
    write_file(cd.df,
               file.path(DIR, "V-Dem-CD", "V-Dem-CD-" %^% VERSION %^% ".csv"))
})

# SPSS
fork12 <- mcparallel({
    info("Creating SPSS CD")
    write_file(set_spss_labels(cd.df, cd_labels),
               file.path(DIR, "V-Dem-CD", "V-Dem-CD-" %^% VERSION %^% ".sav"))
})

# Stata
fork13 <- mcparallel({
    info("Creating Stata CD")
    write_file(set_stata_labels(cd.df, cd_labels),
               file.path(STATA_DIR, "V-Dem-CD-" %^% VERSION %^% ".dta"),
               version = 118,
               data.label = "V-Dem CD")
})




# Make sure there was no error in writing the files
res <- mccollect(list(fork1, fork2, fork3, fork4,
                      fork6, fork7, fork8, fork9,
                      fork10, fork11, fork12, fork13)) %>% unlist
stopifnot(is.null(res))

update_task_status(db = db)
