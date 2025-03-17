#!/usr/bin/env Rscript
# ==========================================================================
# Creates the final public DS with proper variable labels.
# ==========================================================================
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(parallel))
suppressMessages(library(vanalysis))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()
ROOTDIR <- ROOT
DIR <- file.path(ROOTDIR, "dataset")
STATA_DIR <- file.path(ROOTDIR, "dataset_stata")
REFS <- file.path(ROOTDIR, "refs")
VERSION <- Sys.getenv("DS_VERSION")

info(sprintf("Generating %s datasets", VERSION))

tasks <- load_tasks(db)
# -- all dependencies for dataset module
deps <- as.numeric(unlist(
    strsplit(subset(tasks, task_name == "dataset")$deps, ","))) 
stopifnot(length(deps) == length(unique(deps)))

# Load and adjust references
#-------------------------------------------------------------------------------
qtable <- subset(
    load_qtable(),
    !(conthistmerge == "merge" & grepl("^v3", name)) &
    # Do not include variables that are subject to approval (v[23]zz, comments)
    !grepl("approval", datarelease) &
    !grepl("_covid$", name))

# CB_RELEASE is matched against datarelease column in codebook. If the release
# for a question matches the value of the CB_RELEASE it will be included.
codebook <- subset(
    load_codebook(),
    grepl(Sys.getenv("CB_RELEASE"), datarelease)) |> 
    mutate(tag = gsub("\\", "", x = sub(",.*", "", tag), fixed = TRUE)) |> 
    select(question_id, tag, question_number, cb_section, sources)

utable <- load_country_unit()
country <- load_country()
# ------------------------------------------------------------------------------
# Codebook order: Match the ordering of variables in the dataset with the codebook.
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
demed_sections <- c("demed","demedCurrAllSubj","demedCurrSpecSubj",
    "demedmedia","demedschools","demedteachers")
ps_sections <- c("partysystems")
e_sections <- c("e1", "e5", "e6", "e7", "e9", "e12", "e13", "e14", "e15",
                "eb1", "eb2", "region", "eb3", "eb5", "eb6", "eb7", "eb8")
# The ordering of section in cb determines the order in the dataset
cb <- c(vdem, x3_sections, ps_sections, sm_sections, demed_sections, e_sections)

stopifnot(cb %in% codebook$cb_section)

# Merge codebook and qtable
# -- we need cb_section and cb_order
inRow <- nrow(qtable)
qtable <- merge(
    x = qtable,
    y = data.frame(cb = cb, cb_order = seq_along(cb), stringsAsFactors = F),
    by.x = "cb_section", by.y = "cb", all.x = TRUE)
stopifnot(inRow == nrow(qtable))
# question_number is used for ordering of items
qtable <- merge(
    x = qtable,
    y = codebook[, c("question_id", "question_number")],
    by = "question_id",
    all.x = TRUE)
stopifnot(inRow == nrow(qtable))
rm(inRow)

# Set proper name for time-specific country name
qtable$name[qtable$name == "v2histname"] <- "histname"

#-------------------------------------------------------------------------------
# Set variable labels
# -- labels are used for the SPSS and Stata datasets

create_variable_label_table <- function(qtable) {

	qlabels <- qtable %>%
    	filter(is.na(class) | class != "V") %>%
    	select(question_id, name, codebook_name) %>%
    	arrange(name) %>%
    	# Remove backslash from labels
    	mutate(codebook_name = gsub("\\", "", codebook_name, fixed = TRUE)) %>%
    	# adjust multiple selection labels
    	mutate(codebook_name = ifelse(
			grepl("_\\d+$", name),
            codebook_name %^% gsub("^(.*?_)(\\d+)$", " \\2", name),
            codebook_name)) %>%
    	mutate(codebook_name = ifelse(
			name == "v2stfisccap",
            "State fiscal source of revenue", 
			codebook_name)) %>%
    	rename(label = codebook_name)

	qlabels %<>% mutate(label = ifelse(
		name == "e_uds_median", 
		"Unified democracy score posterior",
		label))

	return(qlabels)
}

qlabels <- create_variable_label_table(qtable)
qtable <- subset(qtable, name != "histname")
identifiers <- c('country_name','country_text_id','country_id','year',
    'historical_date','project','historical','histname','codingstart',
    'codingend','codingstart_contemp','codingend_contemp','codingstart_hist',
    'codingend_hist','gapstart1','gapstart2','gapstart3','gapend1','gapend2',
    'gapend3','gap_index', 'COWcode')

# ------------------------------------------------------------------------------
# Read in objects that should go into the dataset
fu <- function(v) {

    out <- list()
    # read file
    tryCatch({
        ll <- read_file(find_dep_file_by_task(v))[[1]]
    }, error = function(err) {
        print(paste0("failed to read task file: ", v))
        return(NULL)
    })
    
    # extract cd and cy
    lapply(c("cd", "cy"), function(lev, ll, v) {

        tryCatch({
            df <- ll[[lev]]

            if (is.null(df)) {
                info(sprintf("No %s data for task %d", lev, v))
                return(NULL)
            }

            df <- df %>%
                add_country_cols(country) %>%
                add_date_cols %>%
                select(-country_text_id)

            if (lev == "cd") {
                df %<>% select(-year)
                stopifnot(`There are duplicates in cd!` = no_duplicates(df,
                    cols = c("country_id", "historical_date")))

            } else if (lev == "cy") {
                df %<>% select(-historical_date)
                stopifnot(`There are duplicates in cy!` = no_duplicates(df, 
                    cols = c("country_id", "year")))
            }
            out[[lev]] <<- df
            
        }, error = function(err) {
            print(paste0("error when extracting objects from task file: ",
                v,
                "error message: ",
                err))
            return(NULL)
        }, warning = function(war) {
            print(paste0("warning when extracting objects from task file: ",
            v,
            "warning message: ",
            war))
            return(NULL)
        })

    }, ll = ll, v = v)
    
    return(out)
}

# Read and extract objects
llf <- mclapply(deps, fu, mc.cores = 8, mc.preschedule = FALSE)

# load cd files
#-------------------------------------------------------------------------------
cd_files <- lapply(llf, function(ll) {return(ll[["cd"]])})
# Remove objects that where not read properly
# -- the reading statement set them to NULL
bool <- unlist(lapply(cd_files, is.null))
missing_deps <- deps[bool]

# print missing cases:
# -- these cases have at least one missing file
# -- some indices are only calculated at a CY level so they will be missing a CD file
subset(tasks, task_id %in% missing_deps, select = c(
    task_name, module_name, status)) |> 
    as.data.frame(stringsAsFactors = F) |>
    print()

cd_files <- cd_files[!bool]
df_cd <- full_join_vdem_tree(cd_files)
rm(cd_files)

# load cy files
#-------------------------------------------------------------------------------
# extract cy data.frames:
cy_files <- lapply(llf, function(ll) {return(ll[["cy"]])})
# Remove objects that where not read properly
# -- the reading statement set them to NULL
bool <- unlist(lapply(cy_files, is.null))
missing_deps <- deps[bool]
# Print missing cases:
subset(tasks, task_id %in% missing_deps, select = c(
    task_name, module_name, status)) |> 
    as.data.frame(stringsAsFactors = F) |>
    print()

cy_files <- cy_files[!bool]
df_cy <- full_join_vdem_tree(cy_files)
rm(cy_files)

# ------------------------------------------------------------------------------
# Save 
write_file(df_cy, file.path(ROOTDIR, "llf", "df_cy.rds"), dir_create = TRUE)
write_file(df_cd, file.path(ROOTDIR, "llf", "df_cd.rds"), dir_create = TRUE)

rm(llf)
# ------------------------------------------------------------------------------
# Read in Exclusion data
# -- merge cd
df_cd <- merge(
    x = df_cd,
    y = subset(read_file(file.path(ROOT, "exclusion", "exclusion_cd.rds")), 
            select = c(-country_name, -year)),
    by = c("country_id", "historical_date"),
    all = TRUE)

# -- merge cy
df_cy <- merge(
    x = df_cy,
    y = subset(read_file(file.path(ROOT, "exclusion", "exclusion_cy.rds")),
            select = c(-historical_date, -country_name)),
    by = c("country_id","year"),
    all = TRUE)

# ------------------------------------------------------------------------------
# Read in Regimes data
# -- merge cd
df_cd <- merge(
    x = df_cd,
    y = subset(read_file(file.path(ROOT, "regimes", "regimes_cd.rds")), 
            select = c(-country_name, -year)),
    by = c("country_id", "historical_date"),
    all = TRUE)

# -- merge cy
df_cy <- merge(
    x = df_cy,
    y = subset(read_file(file.path(ROOT, "regimes", "regimes_cy.rds")),
            select = c(-historical_date, -country_name)),
    by = c("country_id","year"),
    all = TRUE)

# ------------------------------------------------------------------------------
# Read in DEMED/V-Indoc data
# -- merge cd
df_cd <- merge(
    x = df_cd,
    y = subset(read_file(file.path(ROOT, "demed", "demed_cd.rds")), 
            select = c(-country_text_id, -year)),
    by = c("country_id", "historical_date"),
    all = TRUE)

# -- merge cy
df_cy <- merge(
    x = df_cy,
    y = subset(read_file(file.path(ROOT, "demed", "demed_cy.rds")),
            select = c(-historical_date, -country_text_id)),
    by = c("country_id","year"),
    all = TRUE)

# ------------------------------------------------------------------------------
# Read in Party Systems Indices
df_cy <- merge(
    x = df_cy,
    y = select(read_file(file.path(ROOT, "party_systems", "party_systems.csv")),
            country_id, year, starts_with("v2xpas")),
    by = c("country_id", "year"),
    all = TRUE)

stopifnot(!anyNA(df_cy$country_id))
#-------------------------------------------------------------------------------
# Clean loaded files
clean_process <- function(df) {

    print(names(df)[duplicated(names(df))])
    print(grep(".", names(df), fixed = TRUE, value = TRUE))
    # Drop 95% interval
    df <- select(df, -matches("95"))
    # Rename 68 to just codelow or codehigh
    names(df) <- gsub("68", "", names(df))
    
    return(select(df, -matches("_ord_sd$")))
}

# -- prints if there are any duplicated columns
df_cy <- add_country_cols(clean_process(df_cy), country)
df_cd <- add_date_cols(add_country_cols(clean_process(df_cd), country))

stopifnot(no_duplicates(df_cy, c("country_id", "year")))
stopifnot(length(names(df_cy)[duplicated(names(df_cy))]) == 0)
stopifnot(no_duplicates(df_cd, c("country_id", "historical_date")))
stopifnot(length(names(df_cd)[duplicated(names(df_cd))]) == 0)

# sort tables
df_cy <- organiseRows(df_cy, country_id, year, foreground = TRUE)
df_cd <- organiseRows(df_cd, country_id, historical_date, foreground = TRUE)

# Check if the units fit cu table
# --------------------------------------------------------------------------
# Double-Check last utable cleaning
# -- utable is only meaningful for each year. I.e, no country exists for only a fraction of a year

df_cy <- semi_join(df_cy, utable, by = c("country_id", "year"))
df_cd <- semi_join(df_cd, utable, by = c("country_id", "year"))

# Calculate Identifiers
#------------------------------------------------------------------------------
# Define codingstart/end and merge in the rest of our identifiers
idents_df <-
    select(df_cy, country_id, year, histname = v2histname) %>%
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
    ungroup()

# Calculate gaps
# -- identify gaps within utable
# -- note that gap[startend] is limited to only three gaps
assertNumberOfGaps <- function(dat) {
    counts <- table(dat$country_id)
    if (any(counts > 3)) {
        stop("There are countries with more than three gaps in the data")
    }
    return(dat)
}

gaps_df <- select(utable, country_id, year) %>%
    group_by(country_id) %>%
    arrange(year) %>%
    do({
        x <- which(diff(.$year) > 1)
        if (length(x) > 0)
            data.frame(
                country_id = first(.$country_id),
                gapstart = .$year[x] + 1,
                gapend = .$year[x + 1] - 1,
                stringsAsFactors = F)
        else
            data.frame()
    }) %>%
    # -- make sure we capture all gaps, currently identified as three in total
    assertNumberOfGaps() %>% 
    summarise(gapstart1 = gapstart[1], gapstart2 = gapstart[2],
        gapstart3 = gapstart[3], gapend1 = gapend[1], gapend2 = gapend[2],
        gapend3 = gapend[3]) %>%
    ungroup()

# -- remove v2histname from both cd and cy objects
df_cy[["v2histname"]] <- NULL
df_cd[["v2histname"]] <- NULL

#
# Merge ID variables
# --------------------------------------------------------------------------
# -- merge by year
df_cy <-
    add_date_cols(df_cy) %>%
    left_join(idents_df, by = c("country_id", "year")) %>%
    left_join(gaps_df, by = c("country_id"))
df_cd <-
    add_date_cols(df_cd) %>%
    left_join(idents_df, by = c("country_id", "year")) %>%
    left_join(gaps_df, by = c("country_id"))


# Merge e-data into cy
#-------------------------------------------------------------------------------
e_data <- read_file(file.path(ROOT, "external", "e_data.rds"))
stopifnot(no_duplicates(e_data, c("country_id", "year")))
df_cy <- left_join(df_cy, e_data, by = c("country_id", "year"))

df_cd <- left_join(df_cd, select(tt, country_id, year, COWcode),
    by = c("country_id", "year"))

df_cy <- left_join(df_cy, select(tt, country_id, year, COWcode),
    by = c("country_id", "year"))

# Round values to third digit
#-------------------------------------------------------------------------------
df_cy <- round_df(df_cy, 3)
df_cd <- round_df(df_cd, 3)
#-------------------------------------------------------------------------------
# Set NaN to NA
# -- so is.na(NaN) is TRUE. So this call will set every NaN to NA.
is.na(df_cy) <- is.na(df_cy)
is.na(df_cd) <- is.na(df_cd)
# Compare datasets
#-------------------------------------------------------------------------------
if (interactive()) {

    # Check country-year
    old_cy <- read_file(Sys.getenv("OLD_DS_FILE"))
    # what is in new but not in old?
    setdiff(names(df_cy), names(old_cy))
    # What is in old but not in new?
    setdiff(names(old_cy), names(df_cy))
    rm(old_cy)

    old_cd <- read_file(Sys.getenv("OLD_DS_CD_FILE"))
    # what is in new but not in old?
    setdiff(names(df_cd), names(old_cd))
    # What is in old but not in new?
    setdiff(names(old_cd), names(df_cd))
    rm(old_cd)

}

is_vdem_sorted <- function(df) {
    identical(df, dplyr::arrange(df, country_id, historical_date))
}

stopifnot(is_vdem_sorted(df_cy))
stopifnot(is_vdem_sorted(df_cd))

# Do we have the latest year added to both datasets
stopifnot(vapply(list(df_cd, df_cy), function(i) {
    as.numeric(Sys.getenv("NEWEST_YEAR")) %in% i$year }, logical(1)))

# Country-year should only have -12-31
stopifnot(length(unique(substr(unique(df_cy$historical_date), 6, 10))) == 1)

# ------------------------------------------------------------------------------
# Dataset generation section
# Country-Year: Core = All V-Dem indices + components
# -- we only use the CY version of dataset here
info("V-Dem-CY-Core")
dir.create(file.path(DIR, "V-Dem-CY-Core"), recursive = T, showWarnings = F)
dir.create(STATA_DIR, recursive = T, showWarnings = F)

# Select to go into the CORE dataset
indices <- subset(qtable, grepl("^x\\d+", cb_section))$name
comps <- unique(unlist(lapply(indices, function(v) {
    vanalysis::rec_components(v, codebook)
    })))
all_vars <- union(indices, comps)
sorted_vars <- organiseRows(subset(qtable, name %in% all_vars), cb_order, question_number)$name
cy_core_vars <- sort_multiple_selection(unique(unlist(
    lapply(sorted_vars, function(v) {find_vars(v, qtable)}))))

# Report missing columns
missvar <- cy_core_vars[!cy_core_vars %in% colnames(df_cy)]
if (length(missvar) > 0) {
    info(paste0("Missing variables: ", missvar))
}

# -- Include all identifiers and all cy_core_vars
df_country_year_core <- select(
    df_cy,
    all_of(identifiers),
    all_of(cy_core_vars))
info(sprintf("CY rows: %d, cols: %d", 
	nrow(df_country_year_core), ncol(df_country_year_core)))

# -- Set labels
cy_labels <- to_qlabels(colnames(df_country_year_core), qlabels)

# Write dataset files
# rds
fork1 <- mcparallel({
    info("Creating R CY")
    write_file(df_country_year_core,
        file.path(DIR, "V-Dem-CY-Core", paste0("V-Dem-CY-Core-", VERSION, ".rds")))
})

# csv
fork2 <- mcparallel({
    info("Creating CSV CY")
    write_file(df_country_year_core,
        file.path(DIR, "V-Dem-CY-Core", paste0("V-Dem-CY-Core-", VERSION, ".csv")))
})

# SPSS
fork3 <- mcparallel({
    info("Creating SPSS CY")
    write_file(set_spss_labels(df_country_year_core, cy_labels),
        file.path(DIR, "V-Dem-CY-Core", paste0("V-Dem-CY-Core-" ,VERSION, ".sav")))
})

# STATA
fork4 <- mcparallel({
    info("Creating Stata CY")
    write_file(set_stata_labels(df_country_year_core, cy_labels),
        file.path(STATA_DIR, paste0("V-Dem-CY-Core-", VERSION, ".dta")),
        version = 118,
        data.label = "V-Dem-CY-Core")
})

# Country-Year: V-Dem Extended (V-Dem + Other Sources)
#-------------------------------------------------------------------------------
info("V-Dem-CY-Full+Others")
dir.create(file.path(DIR, "V-Dem-CY-Full+Others"), showWarnings = F)

# Select variables
sorted_vars <- organiseRows(subset(
        qtable, cb_section %in% cb & !name %in% c("gapstart", "gapend")),
    cb_order, question_number)$name

cy_extended_vars <- sort_multiple_selection(unique(unlist(
    lapply(sorted_vars, function(v) {find_vars(v, qtable)}))))

# Full+Others is all variables in CY
cy_others.df <- df_cy

# Grab columns
missvar <- cy_extended_vars[!cy_extended_vars %in% colnames(cy_others.df)]
if (length(missvar) > 0) {
    info("Missing variables: " %^% missvar)
}

# qtable %>% filter(qtable$name %in% missvar) %>% view
cy_others.df <- select(cy_others.df, all_of(identifiers), all_of(cy_extended_vars))
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
    gsub("v2edmath_mode", "v2edmath", x = ., fixed = TRUE) %>% 
    gsub("v2edscpatriot_mode", "v2edscpatriot", x = ., fixed = TRUE) %>% 
    gsub("v2edteunion_mode", "v2edteunion", x = ., fixed = TRUE) %>% 
	to_qlabels(qlabels)

# rds
fork6 <- mcparallel({
    info("R CY Full+Others")
    write_file(cy_others.df,
        file.path(DIR, "V-Dem-CY-Full+Others",
        paste0("V-Dem-CY-Full+Others-", VERSION, ".rds")))
})

# csv
fork7 <- mcparallel({
    info("Creating CSV CY-Full+Others")
    write_file(cy_others.df,
        file.path(DIR, "V-Dem-CY-Full+Others",
        paste0("V-Dem-CY-Full+Others-", VERSION, ".csv")))
})

# SPSS
fork8 <- mcparallel({
    info("Creating SPSS CY-Full+Others")
    write_file(set_spss_labels(cy_others.df, others_labels),
        file.path(DIR, "V-Dem-CY-Full+Others",
        paste0("V-Dem-CY-Full+Others-", VERSION, ".sav")))
})

# STATA
fork9 <- mcparallel({
    info("Creating Stata CY-Full+Others")
    write_file(set_stata_labels(cy_others.df, others_labels),
        file.path(STATA_DIR,
        paste0("V-Dem-CY-Full+Others-", VERSION, ".dta")),
        version = 118,
        data.label = "V-Dem CY-Full+Others")
})


# Country-Date: V-Dem, HLIs + all v2 and unique v3
#-------------------------------------------------------------------------------
info("V-Dem-CD Full")
dir.create(file.path(DIR, "V-Dem-CD"), showWarnings = F)

# Read file
cd.df <- df_cd

# Variable selection
# -- we remove some vars manually because they do not exist in CD-format.
sorted_vars <- organiseRows(subset(
        qtable,
        # -- keep these sections
        cb_section %in% c(vdem, x3_sections, sm_sections) &
        # -- drop these variables names
        !name %in% c(
            'gapstart','gapend',
            # -- only CY
            'v2x_regime','v2x_regime_amb',
            'v2x_ex_confidence','v2x_ex_direlect','v2x_ex_hereditary', 'v2x_ex_military','v2x_ex_party',
            'v2xps_party','v2xps_party_codelow', 'v2xps_party_codehigh',
            'v2x_feduni',
            'v2x_accountability', 'v2x_veracc', 'v2x_horacc', 'v2x_diagacc')),
    cb_order, question_number)$name

cd_vars <- sort_multiple_selection(
    unique(unlist(lapply(sorted_vars, function(v) {find_vars(v, qtable)}))))

# Grab columns
missvar <- cd_vars[!cd_vars %in% colnames(cd.df)]
if (length(missvar) > 0) {
    info("Missing variables: " %^% missvar)
}

cd.df <- select(cd.df, all_of(identifiers), all_of(cd_vars))
info(sprintf("CD rows: %d, cols %d", nrow(cd.df), ncol(cd.df)))

# Labels
cd_labels <- colnames(cd.df) %>%
    gsub("v2edmath_mode", "v2edmath", x = ., fixed = TRUE) %>% 
    gsub("v2edscpatriot_mode", "v2edscpatriot", x = ., fixed = TRUE) %>% 
    gsub("v2edteunion_mode", "v2edteunion", x = ., fixed = TRUE) %>% 
	to_qlabels(qlabels)

# rds
fork10 <- mcparallel({
    info("Creating R CD")
        write_file(cd.df, file.path(DIR, "V-Dem-CD",
            paste0("V-Dem-CD-", VERSION, ".rds")))
})

# csv
fork11 <- mcparallel({
    info("Creating CSV CD")
    write_file(cd.df, file.path(DIR, "V-Dem-CD", paste0("V-Dem-CD-", VERSION, ".csv")))
})

# SPSS
fork12 <- mcparallel({
    info("Creating SPSS CD")
    write_file(set_spss_labels(cd.df, cd_labels),
        file.path(DIR, "V-Dem-CD", paste0("V-Dem-CD-", VERSION, ".sav")))
})

# Stata
fork13 <- mcparallel({
    info("Creating Stata CD")
    write_file(set_stata_labels(cd.df, cd_labels),
        file.path(STATA_DIR, paste0("V-Dem-CD-", VERSION, ".dta")),
        version = 118,
        data.label = "V-Dem CD")
})


# Make sure there was no error in writing the files
res <- unlist(mccollect(
    list(fork1, fork2, fork3, fork4, fork6, fork7, fork8, fork9,
        fork10, fork11, fork12, fork13)))
# If there is an error, res does not contain null for that element
stopifnot(is.null(res))
