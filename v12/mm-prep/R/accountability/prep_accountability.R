#!/usr/bin/env Rscript
#
###

suppressMessages(library(dplyr))
library(magrittr)
library(vutils)
library(vanalysis)

db <- pg_connect(Sys.getenv("DS_VERSION"))
ROOT <- Sys.getenv("ROOT_DIR")
OUTFILE <- create_outfile(id = Sys.getenv("TASK_ID"),
                          ROOT = Sys.getenv("ROOT_DIR"),
                          db = db)
varname <- get_varname(id = Sys.getenv("TASK_ID"), db)
objs <- find_dep_files(Sys.getenv("TASK_ID"), db)
country <- read_file(file.path(ROOT, "refs", "country_table.rds"))
utable <- read_file(file.path(ROOT, "refs", "country_unit.rds")) %>%
    select(-historical_date)


df <-
    lapply(names(objs), function(v) {
        objs[[v]][[v]]$cy %>%
            add_country_cols(country) %>%
            add_date_cols
    }) %>% Reduce(full_join_vdem, .)

# Interpolate v2x_elecreg and others
#-------------------------------------------------------------------------------
df %<>%
    fill_special_cols(utable) %>%
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
acc <- df %>%
    left_join(select(utable, country_id, year, project),
              by = c("country_id", "year")) %>%
    filter(project %in% c("contemporary-only", "overlap")) %>%
    select(-project) %>%
    filter(!is.na(v2x_elecreg) & !is.na(v2x_suffr)) %>%
    select(-matches("_codelow|_codehigh|_osp|_ord|_sd|_mean|_nr|project"))


# Set suffrage to 0 when there's no electoral regime. Truncate at .001
# and .999 for jags code.
acc$v2x_suffr[acc$v2x_elecreg == 0] <- 0
acc$v2x_suffr <- pmax(pmin(acc$v2x_suffr, .999), .001)

###
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

# If legislature and executive is elected, obviously HoEel is elected
acc$HoEel <- with(acc, ifelse(v2xlg_elecreg == 1 & v2xex_elecreg == 1, 1, HoEel))

###
# Clean election variables. Start with setting to NA when no electoral
# regime.
elec_vars <- c("v2elembaut", "v2elembcap", "v2elrgstry",
              "v2elirreg", "v2elintim", "v2elfrfair",
              "v2elmulpar")

acc[acc$v2x_elecreg == 0, elec_vars] <- NA

# Impute ELS-only vars (everything except elembaut and
# elembcap). Preserve the NAs from v2x_elecreg == 0.
els <- setdiff(elec_vars, c("v2elembaut", "v2elembcap"))

acc.els <- select(acc, country_id, year, v2x_elecreg, one_of(els)) %>%
    left_join(select(utable, country_id, year, gap_idx),
              by = c("country_id", "year")) %>%
    group_by(country_id, v2x_elecreg, gap_idx) %>%
    arrange(year) %>%
    mutate_at(vars(-group_cols()), locf) %>%
    ungroup %>%
    select(-v2x_elecreg, -gap_idx)

acc %<>% select(-one_of(els)) %>%
    left_join(acc.els, by = c("country_id", "year"))

###
# Cap v2lgbicam at 1
acc$v2lgbicam[acc$v2lgbicam >= 2] <- 1

# write file directly to super
write_file(acc, file.path(Sys.getenv("ACC_SSH_DIR"), "input", "acc_input.rds"))

update_task_status(db = db)
