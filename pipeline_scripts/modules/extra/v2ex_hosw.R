# !/usr/bin/env Rscript

# Construct hosw_rec and hogw_rec (relative power of hos/hog)
#
# Compare HOS vs HOG power appoint cabinet and dismiss ministers.
#
# A couple issues:
#     1. The 0.5 category will practically never be evaluated as TRUE;
#        the probability of the MM output for, as an example,
#        v2exdfdbhs_rec and v2exdjcbhg being exactly the same is so
#        infinitesimally small that this condition is redundant.
#     2. Shouldn't we be using the OSP version of these
#        variables?
#     3. There's a logical inconsistency with how we're handling NA
#        here and what gets set later as hogw_rec. Essentially,
#        hosw_rec gets set when the HOS variable is NOT missing and
#        the HOG variable is; however, the opposite is not true for
#        hogw_rec.
#     4. Lastly, these NA checks are a holdover from the Stata script;
#        they should be mostly fixed already by downstream cleaning,
#        but we'll keep them for now.

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

#
# Functions
# --------------------------------------------------------------------------
make_ex_hosw <- function(df) {

    attach(df)

    hosw1 <- case_when(v2exdfcbhs_rec > v2exdjcbhg | # Appoints of cabinet in practice
                            (!is.na(v2exdfcbhs_rec) & v2exhoshog == 1) ~ 1,
                        v2exdfcbhs_rec == v2exdjcbhg & v2exhoshog == 0 ~ 0.5,
                        v2exdfcbhs_rec < v2exdjcbhg & v2exhoshog == 0 ~ 0)

    hosw2 <- case_when(v2exdfdmhs > v2exdfdshg | # Dismisses ministers in practice
                            (!is.na(v2exdfdmhs) & v2exhoshog == 1) ~ 1,
                        v2exdfdmhs == v2exdfdshg & v2exhoshog == 0 ~ 0.5,
                        v2exdfdmhs < v2exdfdshg & v2exhoshog == 0 ~ 0)

    # Relative power of HOS
    v2ex_hosw <- rowMeans(data.frame(hosw1, hosw2), na.rm = TRUE)

    # Pre-determined scores
    v2ex_hosw <- case_when(country_text_id == "BEL" &
                                historical_date >= "1900-01-01" & historical_date <= "1960-08-09" ~ 0.25,
                            country_text_id == "JAP" &
                                historical_date >= "1900-01-01" & historical_date < "1948-01-01" ~ 1,
                            country_text_id == "MUS" &
                                historical_date >= "1969-01-01" & historical_date < "1993-01-01" ~ 0,
                            country_text_id == "IRN" &
                                historical_date >= "1979-01-01" & historical_date < "1989-01-01" ~ 1,
                            country_text_id == "IRN" &
                                historical_date >= "1989-01-01" ~ 0.5,
                            country_text_id == "DEU" &
                                historical_date > "2014-12-31" ~ 0,
                            country_text_id == "LUX" &
                                historical_date > "2017-12-31" ~ 0,
                            T ~ v2ex_hosw)
    
    outdf <- data.frame(
        country_text_id = df$country_text_id,
        historical_date = df$historical_date,
        v2ex_hosw_calc = v2ex_hosw,
        v2exhoshog = v2exhoshog,
        hosw1 = hosw1,
        hosw2 = hosw2,
        v2exdfcbhs_rec = v2exdfcbhs_rec,
        v2exdjcbhg = v2exdjcbhg,
        v2exdfdshg = v2exdfdshg,
        v2exdfdmhs = v2exdfdmhs,
        stringsAsFactors = FALSE)

    detach(df)

    return(outdf)
}

main <- function(df, utable, objs) {

    # Interpolate hoshog and the rest of the components 
    df <- interpolate_components(df, names(objs), utable, coder_level = FALSE) |> 
        make_ex_hosw()
    return(df)

}

#
# Run functions
# --------------------------------------------------------------------------
if (no_test()) {
    # Global variables
    db <- pg_connect()
    get_globals()

    # Imports
    country <- load_country()
    utable <- load_country_unit()

    objs <- find_dep_files(Sys.getenv("VPIPE_TASK_ID"), db)
    df <- Reduce(full_join_vdem, lapply(names(objs), function(v) {
            add_date_cols(add_country_cols(objs[[v]][[v]]$cd, country))
            }))

    # Run
    collectedInputs <- named_list(df, utable, objs)
    do.call(main, collectedInputs) |> 
    list(cd = _) |> 
    list() |> 
    setNames(TASK_NAME) |> 
    write_file(o=_, f=OUTFILE, dir_create = TRUE)

    } else {
    testthat::test_file("~/proj/vdemds/module_unit_tests/extra/test_v2ex_hosw.R")
    }
