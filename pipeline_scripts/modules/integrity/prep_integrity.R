# !/usr/bin/env Rscript

# ==========================================================================
# Prepares the input object for submit_bfa. It uses the z samples matrices
# from either mm jobs or binary indices. The output is a list of matrices 
# at combined reduced dates.
# ==========================================================================

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()

# Imports
qtable <- load_qtable()
country <- load_country()
utable <- load_country_unit()

objs <- find_dep_files(TASK_ID, db)
comps <- c("v2psbars", "v2psparban", "v2elrstrct", "v2elintim", "v2elmulpar", "v2asuffrage", "v2pepwrses", "v2pepwrsoc", "v2pepwrgen",
        "v2pepwrort", "v2eltrnout", "v2elvaptrn", "v2elrgstry", "v2caassemb",
        "v2x_freexp_altinf", "v2elpeace", "v2dlengage", "v2dlreason", "v2psprlnks",
        "v2smpardom", "v2edplural", "v2dlcountr", "v2edpoledprim", "v2edpoledsec",
        "v2csprtcpt", "v2x_rule", "v2elembaut", "v2eldommon", "v2elintmon", "v2elembcap")
qtable <- subset(qtable, name %in% c(TASK_NAME, comps))
els_vars <- comps[grepl("v2el", comps)]

# join all components
df <- Reduce(full_join_vdem, lapply(names(objs), function(v) {
    add_date_cols(
        add_country_cols(
            objs[[v]][[v]]$cy, country))
})) 

# Interpolate els a-vars
df <- interpolate_components(
    df = df, 
    cols = els_vars, 
    utable = utable,
    keep_nan = FALSE, 
    coder_level = FALSE) 

# remove historical and unnecessary variables and clean by elecreg
model_input <- left_join(df, select(utable, country_id, year, project),
        by = c("country_id", "year")) %>%
    filter(project %in% c("contemporary-only", "overlap")) %>%
    select(-project) %>%
    filter(!is.na(v2x_elecreg)) %>%
    select(-matches("_codelow|_codehigh|_osp|_ord|_sd|_mean|_nr|project"), -v2x_elecreg)

# Recode for stan by normalizing and setting NA to -999
model_input[, comps] <- lapply(model_input[, comps], function(x) {
    x <- as.vector(scale(x))  
    ifelse(is.na(x), -999, x) 
})

write_file(model_input, OUTFILE, dir_create = TRUE)
