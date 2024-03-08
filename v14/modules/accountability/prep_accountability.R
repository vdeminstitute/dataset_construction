#!/usr/bin/env Rscript
#
# Prep data for accountability indices. This script is run once for all accountability models. Accountability indices are only calculated on a country year level.
#-------------------------------------------------------------------------------
suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vanalysis))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()
objs <- find_dep_files(TASK_ID, db)
country <- load_country()
utable <- select(load_country_unit(), -historical_date)

stopifnot(is_path_mounted())

df <-
    Reduce(full_join_vdem, lapply(names(objs), function(v) {
        add_date_cols(
            add_country_cols(
                objs[[v]][[v]]$cy, country))
    }))

# Interpolate v2x_elecreg and others
#-------------------------------------------------------------------------------
df <-
	interpolate_components(
			df = df, 
			cols = c("v2lgbicam", "v2exhoshog", "v2expathhs", "v2expathhg"), 
			utable = utable,
			keep_nan = FALSE, 
			coder_level = FALSE) %>%
    # Cleaning after interpolation
    mutate(v2expathhg = ifelse(v2exhoshog == 1, NA, v2expathhg)) %>%
    left_join(select(utable, country_id, year, gap_idx),
              by = c("country_id", "year")) %>%
    group_by(country_id, gap_idx) %>%
    arrange(historical_date) %>%
    mutate(v2x_elecreg = locf(v2x_elecreg),
         v2xex_elecreg = locf(v2xex_elecreg),
         v2xlg_elecreg = locf(v2xlg_elecreg)) %>%
    ungroup %>%
    select(-gap_idx)


# Remove historical and cleaning
#-------------------------------------------------------------------------------
acc <-
    left_join(df, select(utable, country_id, year, project),
        by = c("country_id", "year")) %>%
    filter(project %in% c("contemporary-only", "overlap")) %>%
    select(-project) %>%
    filter(!is.na(v2x_elecreg) & !is.na(v2x_suffr)) %>%
    select(-matches("_codelow|_codehigh|_osp|_ord|_sd|_mean|_nr|project"))


# Set suffrage to 0 when there's no electoral regime. Truncate at .001
# and .999 for jags code.
acc$v2x_suffr[acc$v2x_elecreg == 0] <- 0
acc$v2x_suffr <- pmax(pmin(acc$v2x_suffr, .999), .001)

# Create index whether HoE is elected
acc %<>%
    mutate(HoSel = ifelse((v2expathhs == 7 & v2xex_elecreg == 1) |
                            (v2expathhs == 6 & v2xlg_elecreg == 1) |
                            (v2ex_legconhos == 1 & v2xlg_elecreg == 1),
                          1, 0),
           HoGel = ifelse((v2expathhg == 8 & v2xex_elecreg == 1) |
                            (v2expathhg == 7 & v2xlg_elecreg == 1) |
                            (v2expathhg == 6 & HoSel == 1) |
                            (v2exaphogp == 1 & v2xlg_elecreg == 1),
                          1, 0),
           HoEel = ifelse(v2ex_hosw %in% c(0, 0.5), HoGel, HoSel))

# If legislature and executive is elected, then HoEel is elected
acc$HoEel <- with(acc, ifelse(v2xlg_elecreg == 1 & v2xex_elecreg == 1, 1, HoEel))

# Clean election variables. Start with setting to NA when no electoral regime.
elec_vars <- c("v2elembaut", "v2elembcap", "v2elrgstry",
              "v2elirreg", "v2elintim", "v2elfrfair",
              "v2elmulpar")

acc[acc$v2x_elecreg == 0, elec_vars] <- NA

# Impute ELS-only vars (everything except elembaut and elembcap). Preserve the NAs from v2x_elecreg == 0.
els <- setdiff(elec_vars, c("v2elembaut", "v2elembcap"))

acc.els <- select(acc, country_id, year, v2x_elecreg, one_of(els)) %>%
    left_join(select(utable, country_id, year, gap_idx),
              by = c("country_id", "year")) %>%
    group_by(country_id) %>% 
    mutate(elecreg_cumidx = create_index(v2x_elecreg)) %>% 
    ungroup() %>% 
    group_by(country_id, gap_idx, elecreg_cumidx) %>%
    arrange(year) %>%
    mutate_at(vars(-group_cols()), locf) %>%
    ungroup %>%
    select(-v2x_elecreg, -gap_idx, -elecreg_cumidx)

acc %<>% select(-one_of(els)) %>%
    left_join(acc.els, by = c("country_id", "year"))

# Cap v2lgbicam at 1
acc$v2lgbicam[acc$v2lgbicam >= 2] <- 1

# write file directly to super
write_file(acc, file.path(Sys.getenv("ACC_SSH_DIR"), "input", "acc_input.rds"))

