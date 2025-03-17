#!/usr/bin/env Rscript

# ------------------------------------------------------------------------------
# Loads downloaded database reference tables and constructs the
# question table which has one row per question/variable.
# ------------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(tidyr))
suppressMessages(library(DBI))
suppressMessages(library(vbase))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# -- Get database for pipeline
db <- pg_connect()
# -- Assign global variables
get_globals()

INDIR <- file.path(ROOT, "download")
OUTDIR <- file.path(ROOT, "refs")

# Imports
codebook <- read_file(file.path(INDIR, "codebook.rds"))
question_type <- read_file(file.path(INDIR, "question_type.rds"))
choice_tbl <- read_file(file.path(INDIR, "choice.rds"))
choice <- choice_tbl
vignette_parent <- read_file(file.path(INDIR, "vignette_parent.rds"))
question <- read_file(file.path(INDIR, "question.rds"))
question_queue_tbl <- read_file(file.path(INDIR, "question_queue.rds"))
question_queue <- question_queue_tbl
dichotomised_ids <- read_file(file.path(INDIR, "dichotomised_ids.rds"))
survey_tbl <- read_file(file.path(INDIR, "survey.rds")) 
survey <- survey_tbl

# Functions=====================================================================
clean_codebook <- function(codebook) {
    codebook %>%
        mutate(sources = gsub("[\\]", "", sources)) %>%
        mutate(tag = gsub("[\\]", "", tag)) %>%
        mutate(name = gsub("\\", "", name, fixed = T)) %>%
        mutate(clean_tag = gsub("^(.*?)[,].*$", "\\1", tag)) %>%
        filter(!is.na(question_id)) %>%
        filter(!is.na(datarelease)) %>%
        mutate(codebook = TRUE) %>%
        mutate(responses = gsub("<br>", "\n", responses, fixed = T)) %>%
        mutate(responses = gsub("\\\\", "\n", responses, fixed = T)) %>%
        mutate(responses = gsub("\n ", "\n", responses, fixed = T)) %>%
        select(question_id, sources, crosscoder_aggregation, datarelease,
            aggregation, cy_aggregation, scale, responses, codebook_tag = tag, cb_section,
            vartype, clean_tag, codebook, conthistmerge, histmerged,
            hist_outside_coding, additional_versions, date_specific, cleaning,
            cont_outside_coding, codebook_name = name,
            overlap_use_hist, is_party, no_update) %T>%
        {stopifnot(length(unique(.$question_id)) == nrow(.))} %>%
    return(.)
}

# This functions creates columns that decides how variables are merged across
# contemporary and historical projects
identify_hist_merge_procedure <- function(qtable) {
    # All historical variables
    allhist <-
        qtable %>%
        filter(grepl("^v3", name)) %$% name

    # indivhist are historical variables where there exist no v2 equivalents
    indivhist <-
        qtable %>%
        filter(conthistmerge == "hist_only") %$% name

    hist_no_want_merge <-
        qtable %>%
        filter(conthistmerge == "no_merge") %$% name

    hist_merge_conflict <-
        qtable %>%
        filter(conthistmerge == "merge_conflict") %$% name

    hist_merge_no_conflict <-
        qtable %>%
        filter(conthistmerge == "merge") %$% name

    qtable %<>%
        mutate(indivhist = name %in% indivhist,
               hist_no_want_merge = name %in% hist_no_want_merge,
               hist_merge_conflict = name %in% hist_merge_conflict,
               hist_merge_no_conflict = name %in% hist_merge_no_conflict)

    # Which variables are merged historical and contemporary
    qtable %<>% rename(hist_merged = histmerged)
    return(qtable)
}

add_dichotomized_questions <- function(qtable, dich_df) {
    info("Remove old questions that are the dichotomized versions, ")
    info("we will autogenerate them...")
    info("Removing dichotomized questions...")
    rem_dich_df <- 
        inner_join(
            rename(qtable, dich_parent_tag = name),  
            rename(dich_df, question_id_dich = question_id), 
            by = c("question_id" = "parent_question_id")) %>%
        select(-question_id) %>%
        rename(question_id = question_id_dich)  %>%
        mutate(dichotomized = TRUE) %>%
        filter(!grepl("_rec$", name))

    info("Generating dichotomized versions")
    qtable %<>% 
        bind_rows(rem_dich_df) %>%
        mutate(dichotomized = ifelse(is.na(dichotomized), FALSE, TRUE)) %>%
        mutate(to_dichotomize = grepl("S", question_type)) %>%
        mutate(to_dichotomize = ifelse(name %in% c("v2casoe", "v2csstruc"), 
            TRUE, to_dichotomize))

    q_dich <- qtable %>% 
        filter(to_dichotomize, !grepl("_\\d+", name), grepl("v\\d", name),
        !(grepl("v3", name) & conthistmerge == "merge")) %>%
        select(question_id, name, cb_responses) %>%
        mutate(choices = strsplit(cb_responses, " "))
    v <- Map(function(choi, name) {
            name %^% "_" %^% choi
        }, choi = q_dich$choices, name = q_dich$name) %>% unlist
    if (any(!v %in% qtable$name)) {
        info("Some dichotomized questions are missing, add entries in dichotomised_ids: " %^%
            v[!v %in% qtable$name])
        stop("Error")
    }

    return(qtable)
}

merge_question_codebook <- function(question, codebook) {
	left_join(question, codebook, by = "question_id") %>%
    mutate(is_party = case_when(
        grepl("^A_v2pa", name) ~ TRUE,
        is.na(is_party) ~ FALSE,
        TRUE ~ is_party)) %>%
    filter(!is_party)
}

add_answer_categories_k <- function(qtable, qcats) {
	qtable %>%
    left_join(qcats, by = "question_id") %>%
    mutate(k = as.numeric(k)) %>%
    mutate(k = case_when(
		# Binary questions have two answer categories
		question_type == "Y" ~ 2,
		# Questions that also have a binary version get
		# one answer category removed
        name %^% "bin" %in% name ~ k - 1,
        TRUE ~ k))
}

# the regular expression captures several potential scenarios, including
# situations with fractions (0, 0.25, 0.5, 1, v2ex_hosw)
get_codebook_answer_categories <- function(qtable) {
	info("Get codebook answer categories...")
	qtable %<>%
    	group_by(question_id) %>%
    	mutate(cb_responses =
    	    str_extract_all(responses, "^\\d|(\\n\\d+(\\.\\d+)?)") %>%
    	    unlist %>%
    	    gsub("\n", "", ., fixed = T) %>%
    	    paste(collapse = " ")) %>%
    	ungroup()
	is.na(qtable$cb_responses) <- qtable$cb_responses %in% c("", " ", "NA")
	return(qtable)
}

fix_class <- function(qtable, vignette_parent) {
	# Set class to E for Ordinal Versions of Indices
	qtable$class[qtable$cb_section == "e1"] <- "E"
	qtable$vartype[qtable$cb_section == "e1"] <- "E"

	# Create class V for vignettes
	qtable$class[qtable$question_id %in% vignette_parent$question_id] <- "V"
	qtable$class[grepl("^(A_)|(B_)", qtable$name)] <- "V"

	# If class is D, set question_type to N
	qtable %<>% mutate(question_type = ifelse(class == "D", "N", question_type))

	qtable %<>%
	    mutate(question_type = ifelse(grepl("^e_v2x.*(c|C)", name), "N", question_type),
	        class = ifelse(grepl("^e_v2x.*(c|C)", name), "E", class))

	# Remove unnecessary questions by question_type
	qtable %<>% filter(!question_type %in% c("X", "O", "Q"))

	## ZZ gets new class
	qtable$class[grepl("^v\\dzz", qtable$name)] <- "Z"

	# Set class to E for last e-variables
	qtable %<>% mutate(class = ifelse(is.na(class) &
                                    grepl(Sys.getenv("CB_RELEASE"), datarelease) &
                                    grepl("^e_", name) &
                                    !grepl("v2x", name),
                                 "E", class))

	# Everything should have a class
	stopifnot(qtable %>% filter(grepl(Sys.getenv("CB_RELEASE"), datarelease)) %$% !is.na(class))

	return(qtable)
}

# Link vignettes for same questions across projects and fills class from 
# codebook:vartype.
misc_stuff <- function(qtable) {

	# Everything except vignettes should have a cb_section
	stopifnot(qtable %>% filter(class != "V") %$% !is.na(cb_section))

	# Create parent tag
	qtable$parent_tag <- gsub("^v\\d", "", qtable$name)

	qtable$class[is.na(qtable$class) & !is.na(qtable$vartype)] <-
	    qtable$vartype[is.na(qtable$class) & !is.na(qtable$vartype)]
	return(qtable)
}

# identifies components for indices
identify_components <- function(qtable) {
	# Set components to NA if it is not contained in name
	qtable %<>% rename(components = sources)
	v <-
	    qtable %>%
	    filter(!dichotomized) %>%
 	    filter(class != "V") %$%
	    paste(name, collapse = "|")
    # Set components to missing for:
	is.na(qtable$components) <-
        # 1) if they do not contain any variable name (v)
	    !grepl("(" %^% v %^% ")", qtable$components) |
        # 2) if they contain the following special characters
	        grepl("[,:;({})]", qtable$components) |
        # 3) if they begin with a capital letter
	        grepl("^[A-Z]", qtable$components)

	# Find if a variable is a component for an index
    # Can contain multiple values
	qtable$index_component <-
	    lapply(qtable$name,
	           function(na) {
	               paste(na.omit(qtable$name[grepl(na, qtable$components)]),
	                    collapse = " ")
	           }) %>% unlist
	is.na(qtable$index_component) <- qtable$index_component == ""
	return(qtable)
}

# identify the type of estimation for indices: mm, bfa, bfa_perc, binary, hli
identify_index_types <- function(qtable) {
	qtable %>% 
		mutate(mm_prep = grepl("Bootstrapped|Bayesian item response theory|Mean",
                               crosscoder_aggregation)) %>%
		mutate(percent = grepl("Bootstrapped", crosscoder_aggregation)) %>%
		mutate(mm = grepl("Bayesian item response theory",
                              crosscoder_aggregation,
                              fixed = T)) %>%
		mutate(mm = ifelse(hist_merge_no_conflict, F, mm)) %>%
		mutate(NA_historical_date = grepl("D",
                                          question_type,
                                          fixed = TRUE)) %>%
		mutate(binary_index = ifelse(
			name %in% c("v2xcl_acjst", "v2xcl_disc", "v2xcl_dmove",
						"v2xcl_prpty", "v2xcl_slave",
       					"v2x_clphy", "v2x_pubcorr", "v2x_execorr"), 
			TRUE, FALSE)) %>%
		# Determine BFAs
		mutate(bfa = grepl("Bayesian factor analysis", aggregation)) %>%
    	mutate(bfa = ifelse(binary_index | name == "v2x_civlib", FALSE, bfa)) %>%
		# Determine HLIs
		mutate(hli = grepl("v2x", components) &
                            cb_section %in% c("x1", "x2")) %>%
		mutate(bfa_perc = name %in% c("v2xeg_eqprotec", "v2x_gencs"))
}

# determines what goes into the coder_level dataset
determine_disaggregated <- function(qtable) {
	qtable %>% mutate(
        disaggregated = grepl("disaggregated dataset|Available upon request",
            datarelease) |
        (class == "C" & question_type == "T") |
        (class == "A,B,C"))
}

# identifies which column, text_answer/code that holds the expert response
identify_code_text_answer <- function(qtable) {
	qtable %<>%
		mutate(code_col = question_type %in% c("M", "N", "V", "Y", "R", "U"),
 	          text_col = question_type %in% c("T", "S", "D"))
	qtable$code_col[qtable$dichotomized] <- TRUE
	qtable$text_col[qtable$dichotomized] <- FALSE
	return(qtable)
}

# creates a recoded version of an already existing variable
identify_recoded_variables <- function(qtable, dich_df) {
	qtable %<>% mutate(recoded = name %in% "v2exdfcbhs")
	# Create question table entries for recoded variables
	qtable_rec <- qtable %>%
	    filter(recoded) %>%
	    mutate(name = name %^% "_rec") %>%
	    ungroup() %>%
	    select(-question_id)
	qtable_rec %<>% inner_join(dich_df, by = "name")

	qtable %<>%
	    filter(!name %in% qtable_rec$name) %>%
	    bind_rows(qtable_rec)
	qtable$dichotomized[grepl("_rec$", qtable$name)] <- FALSE
	qtable$k[qtable$name == "v2jupack"] <- 4

	return(qtable)
}

# recodes a single category into its own binary variable
recode_category_to_binary <- function(qtable) {
	info("From some variables we generate a binary variable only from one category!")
	qtable %<>%
	    mutate(recode_category_to_binary =
	        case_when(name == "v2mecenefi" ~ 0,
	                  name == "v2lgdsadlo" ~ 0,
	                  name == "v2elffelr" ~ 5,
	                  TRUE ~ NA_real_))
	info("for now we already have them in the question table.")

    # 'binary_var' says if that variable is a binary version
	qtable %<>% mutate(binary_var = ifelse(
	    name %in% c("v2mecenefibin", "v2lgdsadlobin", "v2elffelrbin"),
	    TRUE, FALSE))
	return(qtable)
}

adjust_cb_responses <- function(qtable) {
	qtable %<>%
	    group_by(parent_tag) %>%
	    mutate(cb_responses = ifelse((hist_merge_conflict | hist_merge_no_conflict) &
	                                    is.na(cb_responses) &
	                                    grepl("^v3", name),
	                                 na.omit(unique(cb_responses)),
	                                 cb_responses)) %>%
	    ungroup

	qtable %<>%
	    mutate(cb_responses = case_when(
	        name == "v2elffelr" ~ "0 1 2 3 4 5",
	        name == "v2mecenefi" ~ "0 1 2 3 4",
	        name == "v2lgdsadlo" ~ "0 1 2 3 4 5",
	        TRUE ~ cb_responses))
	return(qtable)
}

# for which variables should their be vignettes
identify_vignetted <- function(qtable) {
	qtable %<>% mutate(vignetted = mm)
	qtable$vignetted[grepl("_rec$", qtable$name)] <- FALSE
	qtable$vignetted[grepl("_\\d+$", qtable$name)] <- FALSE
	return(qtable)
}

intermediary_checks <- function(qtable) {

	# Any C-question with minimum value above 0?
	bool <- qtable$class == "C" & qtable$min > 0 & !is.na(qtable$min) &
		(!(qtable$dichotomized | qtable$to_dichotomize))
	if (any(bool)) {
		stop("There are C-questions with minimum value above 0!")
	}

	stopifnot(`class contains missing values` = !is.na(qtable$class))
	stopifnot(`name contains missing values` = !is.na(qtable$name))

    # should not exist
	stopifnot({
    	qtable %>%
    	filter(class == "C",
            question_type != "T",
            !disaggregated,
            is.na(crosscoder_aggregation),
            !hist_merged) %>%
            nrow == 0
	})

	# define which v3 variables that are merged, switch v3 for v2, then check
    # if the v2-varible is actually flagged as hist_merged (TRUE/FALSE)
	v3mergevars <- qtable %>%
        # hist_merge_conflict and hist_merge_no_conflict exhaust all v3-variables
        # that should be merged with its v2-equivalent 
	    filter(hist_merge_conflict | hist_merge_no_conflict) %$% name
	v2mergevars <- gsub("v3", "v2", x = v3mergevars, fixed = TRUE)
	stopifnot(`v3-version is to be merged, but hist_merged for v2-version disagrees`
        = {qtable %>% filter(name %in% v2mergevars) %$% hist_merged })

	return(qtable)
}

# remove unwanted pattern in names and recode name of ordinalized index versions
clean_name_question_type <- function(qtable) {
	qtable %>%
	    mutate(name = gsub("/", "", name, fixed = TRUE)) %>%
	    mutate(name = gsub("^(.*?)(_\\d)c.*$", "\\1\\2C", name)) %>%
	    mutate(name = gsub("^(.*?)(_\\d)C.*$", "\\1\\2C", name)) %>%
    	mutate(question_type = ifelse(is.na(question_type) & grepl("^e_", name),
    								  "E", question_type))

}

add_survey <- function(qtable, survey_tbl, question_queue_tbl) {
	
    # Keep active survyes and not V-Party
    survey <-
    	survey_tbl %>%
    	filter(category == 28) %>%
    	filter(!grepl("(NV)", name, fixed = TRUE)) %>%
    	filter(!grepl("Party-Testing", name, fixed = TRUE)) %>%
    	filter(!grepl("Party Identity & Organization", name, fixed = TRUE)) %>%
    	filter(!grepl("Historical - All Surveys", name, fixed = TRUE)) %>%
    	filter(!grepl("Country Coordinator", name, fixed = TRUE))

    # which question_id exists for which survey
	question_queue <- question_queue_tbl %>%
	    inner_join(select(survey, survey_id), by = "survey_id") %>%
	    inner_join(select(qtable, question_id), by = "question_id") %>% 
        left_join(., select(survey, survey_id, survey_name = name),
			by = "survey_id") %>%
	    select(question_id, survey_id, survey_name)
	stopifnot(no_duplicates(question_queue, "question_id"))

    # identifies which surveys that contains sequential codings
	qtable %<>%
        left_join(question_queue, by = "question_id") %>% 
	    mutate(backfill_question =
	        (survey_id %in% c(2057,2058,2059,2060,2061,2062,2063,2064,2065,2066,2097)) &
	        grepl("C", class)) %>%
	    mutate(backfill_question = ifelse(is.na(backfill_question), FALSE,
	        backfill_question))

	return(qtable)
}

correct_downloaded_tables <- function(qtable) {
    # Note that v2ex_hosw has class A. Hence, we already download it from a_data.
	qtable$rating[qtable$class %in% c("D", "E")] <- FALSE
	qtable$a_data[qtable$class %in% c("D", "E")] <- FALSE
	qtable$a_data[grepl("^v2xdd_", qtable$name)] <- TRUE
	qtable$a_data[grepl("_elecreg$", qtable$name)] <- TRUE
	qtable$a_data[grepl("abort", qtable$name)] <- TRUE

	qtable %<>% arrange(name)
	return(qtable)
}

drop_variables <- function(qtable) {
	# Drop if
	qtable <-
        # Keep if vignette, contemporary, history, external OR id variables
		filter(qtable, grepl("^A_|^B_|^v2|^v3|^e_", name) | is_id == TRUE) %>%
        # Do not keep PSQ data
		filter(!grepl("^v2zz|^v3zz", name))

    # Drop non-public variables
	qids_remove <-
	    filter(qtable, grepl("Available upon request", datarelease)) %$%
            question_id
	qtable <- filter(qtable, !question_id %in% qids_remove)

	return(qtable)
}

adjustments_for_dichotomous_variables <- function(qtable) {
	# set to_dichotomize to FALSE for dichotomized versions
	qtable %<>%
	    mutate(to_dichotomize = ifelse(grepl("_\\d+$", name) & class != "V",
	                                   FALSE,
	                                   to_dichotomize))
	qtable$dichotomized[is.na(qtable$dichotomized)] <- FALSE

	# Adjust number of answer categories for recoded and dichotomous variables:
	qtable %<>%
	    mutate(k = case_when(
	        grepl("_\\d+$", name) ~ 2,
	        grepl("_rec$", name) ~ 3,
	        question_type == "R" ~ 100,
	        name == "v3elffelr" ~ 5,
	        TRUE ~ k))
	return(qtable)
}

last_checks <- function(qtable) {
	stopifnot(!anyNA(qtable$question_id))
	stopifnot(length(unique(qtable$question_id)) == nrow(qtable))
	stopifnot(!any(grepl(".", names(qtable), fixed = TRUE)))
	stopifnot(!apply(qtable[, c("rating", "a_data")], 1, all)) 

	# check that all mm variables have 0 as lowest score:
	stopifnot({
	    qtable %>%
	        filter(mm) %$%
	        grepl("^0", cb_responses)
	})
	return(qtable)
}

# ------------------------------------------------------------------------------
# Start creating qtable 

# -- Clean codebook table
codebook <- clean_codebook(codebook)

# -- Get question_type and question_type_label
question_type %<>% 
    mutate(question_type_label =
        tolower(gsub("[ //]", "_", question_type_label))) %>%
    select(question_type = question_type_short, question_type_label)

# -- Get choice values
choice %<>%
    group_by(question_id) %>%
    arrange(choice_value) %>%
    mutate(choice_values = paste(choice_value, collapse = " ")) %>%
    filter(row_number() == 1L) %>%
    bind_rows(
        data.frame(question_id = c(4537, 4538),
            choice_values = c("0 1 2 3 4 5 6", "0 1 2 3 4"))
        ) %>%
    select(question_id, choice_values)

# -- Get which variables are vignettes
# -- any id present in vignette_parent$vignette_question_id will be identified as a vignette
vignette_parent <- select(vignette_parent, question_id = vignette_question_id)

# -- Get min choice value
qcats <-
    choice_tbl %>%
    group_by(question_id) %>%
    summarise(min = min(choice_value), k = n())

# -- Get question table
question <-
    mutate(question, class = toupper(class)) %>%
    mutate(question_type = ifelse(grepl("^B_", name), "V", question_type)) %>%
    select(question_id, name, question_type, index_type, class, is_id, archived)

# -- Get question queue, it can be used to determine which questions are in which surevy?
question_queue <- select(question_queue, question_id, survey_id)

# -- Identify active surveys (category == 28)
survey <-
    select(survey, survey_id, category) %>%
    filter(category == 28)

# -- Determine active questions in online interface
active_questions <- inner_join(question_queue, survey, by = "survey_id")$question_id

# -- Load Predefined dichotomous question ids
dich_df <- 
	dichotomised_ids %>%    
    inner_join(
        select(codebook, question_id, is_party) %>% 
            filter(!is_party) %>% 
            select(-is_party),
        by = c("parent_question_id" = "question_id")) %>%
    rename(question_id = child_question_id)

# Create qtable
# -- Merge imported table and remove party variables and vignettes
qtable <- 
	merge_question_codebook(question, codebook) %>%
	# Merge question_type labels
	left_join(question_type, by = "question_type") %>%
	# Identify active questions (active in the online coding interface)
    mutate(active = ifelse(question_id %in% active_questions, TRUE, FALSE)) %>%
	# Add number of answer categories k
	# We need these for the measurement model
	add_answer_categories_k(qcats) %>%
	# Remove questions without answers
	filter(question_type != "O") %>%
	# Remove party questions
	filter(!grepl("^v2pa", name)) %>%
	# Fix class for certain variables (e.g. vignettes))
	fix_class(., vignette_parent) %>%
	# Keep only variables that have a datarelease or are vignettes
	filter(grepl(Sys.getenv("CB_RELEASE"), datarelease) | class == "V") %>%
	get_codebook_answer_categories(.) %>%
	# Add dichotomized questions
	# We assign new question_ids from the dichotomised_ids table!
	add_dichotomized_questions(., dich_df) %>%
	# Set class when missing
	misc_stuff(.) %>%
	# The codebook table tells us which variables are merged 
	# between historical and contemporary
	identify_hist_merge_procedure(.) %>%
	# Filter away those without datarelease
    filter(!is.na(datarelease) | class == "V",
        grepl(Sys.getenv("CB_RELEASE"), datarelease) | class == "V") %>%
	# Identify index components from the codebook
	identify_components(.) %>%
	identify_index_types(.) %>%
	# Join space separated choice_values
	left_join(choice, by = "question_id") %>%
	# Identify variables that only go into the disaggregated dataset
	determine_disaggregated(.) %>%
	# How are variables calculated
	mutate(calculation = ifelse(mm , "mm", NA)) %>%
	mutate(calculation = ifelse(bfa, "bfa", calculation)) %>%
	# From which table should the variables be downloaded?
    mutate(rating = 
		(!class %in% c("A*", "A", "A,C", "B", "B,C", "A,B", "D")) & !is_id) %>%
    mutate(a_data = 
		(class %in% c("A*", "A", "A,C", "B", "B,C", "A,B", "D")) & !is_id) %>%
	# Which columns are the data values in?
	identify_code_text_answer(.) %>%
	# Multiple selection questions are coded into categories
	identify_recoded_variables(., dich_df) %>%
	# Some answer categories are split off as separate variables
	recode_category_to_binary(.) %>%
	adjust_cb_responses(.) %>%
	# Which variables should have vignettes?
	identify_vignetted(.) %>%
	# Rename v4 variables
	mutate(name = gsub("^v4", "v2", name)) %>%
	# Set els for election-data specific variables
	mutate(els = grepl("v\\d{1}eltype|^Election-specific", date_specific)) %>%
	# Add hos/hog-appointment specific flag [aps] that is true for positive cases
	mutate(aps = grepl("^Coded on HO[GS] appointment", date_specific)) %>%
	intermediary_checks(.) %>%
	clean_name_question_type(.) %>%
	# Some D variables are downloaded, others not
	correct_downloaded_tables(.) %>%
	drop_variables(.) %>%
	add_survey(., survey_tbl, question_queue_tbl) %>%
	adjustments_for_dichotomous_variables(.) %>%
	last_checks(.) %>% 
    as.data.frame() %>% 
    organiseRows(question_id)

# -- Results
info(sprintf("Created qtable with %d items", nrow(qtable)))
info(sprintf("Including %d of class C, %d of class A, A*, B or A,C, and %d of class D",
    sum(qtable$class == "C"),
    sum(qtable$class %in% c("A", "A*", "B", "A,C")),
    sum(qtable$class == "D")))
info(sprintf("Including %d vignettes, where %d are archived.",
    sum(qtable$class == "V"), sum(qtable$archived[qtable$class == "V"])))
info("Adding qtable to the pipeline database as 'qtable'.")
vbase::pg_create_table(df = qtable, name = "qtable", db = db, overwrite = TRUE)
invisible(suppressWarnings(DBI::dbSendStatement(conn = db, statement = "ALTER TABLE qtable OWNER TO vdem;")))
info("Saving qtable to the refs directory.")
vbase::write_file(qtable, file.path(OUTDIR, "question_table.rds"), dir_create = TRUE)

# -- Set attributes
attr(qtable, "meta_data") <- c(
    question_id = "Question identifier number from database",
    name = "Variable tag",
    question_type = "Multiple choice, multiple selection, etc. Cf. question_type_label",
    index_type = "Dichotomous, interval, etc.", 
    class = "A*, B, C, etc.",
    els = "Identifier for whether a variable is election-date specific",
    aps = "Identifier for whether a variable is HOG/HOS appointment-date specific",
    is_id = "Is this variable an identifier variable?",
    components = "Index component variables",
    crosscoder_aggregation = "How are coder scores aggregated?",
    datarelease = "Release version and release notes",
    aggregation = "Text clarifying index aggregations.",
    cy_aggregation = "Method of CD to CY aggregation of Non-C variables",
    scale = "Ordinal, interval, etc.", 
    responses = "Question response categories",
    codebook_tag = "The codebook tag may be different, with escaped underscores",
    cb_section = "Codebook section identifier used in the codebook latex template",
    vartype = "Variable type according to codebook table",
    clean_tag = "Codebook variable tag with escaping removed",
    codebook = "Does this variable have a codebook entry?",
    conthistmerge = "This is specified for historical variables only: hist_only=This variable does not exist as contemporary; merge=Fully merge this variable with the contemporary equivalent; merge_conflict=Merge this with v2, but keep v3 as well;no_merge=This variable exists as v2, but we do not want to merge.", 
    hist_merged = "Identify which v2 variables are merged with v3 (for vignettes)",
    hist_outside_coding = "Do historical variables have observations outside their official coding period as defined by country_unit table?",
    additional_versions = "Variable versions for codebook entry",
    date_specific = "Is this variable coded only on specific dates?",
    cleaning = "Is this variable being cleaned by another variable?", 
    cont_outside_coding = "Do contemporary variables have observations outside their official coding period as defined by country_unit table?",
    codebook_name = "Variable description according to codebook", 
    overlap_use_hist = "Use v3 instead of v2 data for overlap period.",
    question_type_label = "Label for question types",
    active = "Is this question active in the online survey interface?",
    min = "Minimum value. Relevant for MM variables starting at 1.",
    k = "Number of answer categories",
    cb_responses = "Question responses as in codebook", 
    dich_parent_tag = "Tag of parent variable for dichotomized variables", 
    dichotomized = "Is this variable a dichotomized version of another?",
    to_dichotomize = "Will other variables be created from this one as dichotomized version?",
    parent_tag = "Variable tag without v2 or v3 prefix",
    indivhist = "Created from conthistmerge where hist_only is true",
    hist_no_want_merge = "Created from conthistmerge where no_merge",
    hist_merge_conflict = "Created from conthistmerge where merge_conflict",
    hist_merge_no_conflict = "Created from conthistmerge where merge",
    index_component = "Not in use. In which indices is this variable used?",
    mm_prep = "Does this variable follow the mm_prep scripts?",
    percent = "Percentag variable",
    mm = "Does this variable go through the mm?",
    NA_historical_date = "NA in historical_date for all observations.",
    binary_index = "Calculated with the binary_index script",
    bfa = "Bayesian Factor Analysis. Not in use?",
    hli = "High level indicate. Not in use?",
    choice_values = "Question choice values from choice table.",
    disaggregated = "Old. Does this variable go into the disaggregated dataset.",
    calculation = "Calculation method. Not in use?",
    rating = "Variable stored in rating table",
    a_data = "Variable stored in a_data table",
    code_col = "Data stored in code column", 
    text_col = "Data stored in text answer column",
    recode_category_to_binary = "Is an answer category recoded to a separate variable?",
    binary_var = "Is this a variable that was created from the answer category of another variable?",
    vignetted = "Vignettes exist for this variable",
    survey_id = "Survey identifier for online survey interface",
    survey_name = "Survey name for online survey interface",
    backfill_question = "Is this question being backfilled (sequential coding)",
    archived = "Is this question archived at the online coding interface?"
    ) 
