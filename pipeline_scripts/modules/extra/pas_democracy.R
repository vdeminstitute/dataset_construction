#!/usr/bin/env Rscript
# --------------------------------------------------------------------------
# This script updates the v2xpas_democracry indices with the latest version of polyarchy
# NOTE other v2xpas indices do not need to be updated as they only use vparty data
# --------------------------------------------------------------------------
suppressMessages(library(vutils))
suppressMessages(library(dplyr))
suppressMessages(library(vpipe))

db <- pg_connect()
get_globals()

objs <- find_dep_files(TASK_ID, db)[["v2x_polyarchy"]][["v2x_polyarchy"]]$cy
vparty <- read_file("~/data/vparty02/dataset/V-Dem-CPD-Party/V-Dem-CPD-Party-V2.rds")  

polyarchy <- objs %>%
    # Drop 95% interval
    select(-matches("95")) %>% 
    # Rename 68 to just codelow or codehigh
    rename_with(~ gsub("68", "", .))

vars <- c("country_id", "country_text_id", "year", "historical_date", "v2pagovsup", "v2paseatshare", "v2paenname", "v2pavote", "v2x_polyarchy", "v2xpa_antiplural")

vparty <- vparty %>%
    select(any_of(vars)) %>% 
    left_join(polyarchy, by = c("country_text_id", "year")) %>%
    filter(year > 1969 & year < 2020)

# variable on whether the party is in opposition (3) or government (1 or 2)
# 1 = governemnt, 0 = opposition
vall <- vparty %>%
    mutate(v2pagovsup.dum = case_when(
        v2pagovsup == 0 ~ 1,
        v2pagovsup == 1 ~ 1,
        v2pagovsup == 2 ~ 1,
        v2pagovsup == 3 ~ 0,
        TRUE ~ NA)) 

## replacing NAs on seatshare for Democratic social Party Brazil with A data manually
## NOTE TO READER: This passage is important for research on Brazil as the years between
## 1980-1990 overlaps with the democratization of the country. Out of the many political
## parties that existed at the time, only the A-data on seat share of the Democratic Social Party
## is missing from V-Party. Leaving these observations as NAs would result in dropping three 
## key years. We replace them manually using the following source:
## Dieter Nohlen (2005) Elections in the Americas: A data handbook, Volume II
vall <- vall %>%
    mutate(
        v2paseatshare = case_when(
            country_id == 19 & v2paenname == "Democratic Social Party" & year == 1982 ~ 43.22,
            country_id == 19 & v2paenname == "Democratic Social Party" & year == 1986 ~ 7.89,
            country_id == 19 & v2paenname == "Democratic Social Party" & year == 1990 ~ 8.91,
            TRUE ~ v2paseatshare),
        seatshare_1 = v2paseatshare*0.01,
        pavote_1 = v2pavote*0.01)

# Creating our new index: Party-System Democracy Index
psdi_raw <- vall %>%
    group_by(country_id, country_text_id, year, historical_date, v2pagovsup.dum, v2x_polyarchy, v2x_polyarchy_codelow, v2x_polyarchy_codehigh) %>% 
    summarise(wm_autpol = weighted.mean(v2xpa_antiplural, seatshare_1),
        .groups = 'drop') %>%
    as.data.frame()

# pivot 
# -- how are NAs supposed to be handled here (NAs coming from cases of polyarchy has values and v2pagovsup.dum does not)
psdi_data <- psdi_raw %>%
    long_to_wide(c("country_id", "country_text_id", "year", "historical_date", "v2x_polyarchy", "v2x_polyarchy_codelow", "v2x_polyarchy_codehigh"), "v2pagovsup.dum", "wm_autpol") %>%
    rename(government = "1",
        opposition = "0") %>%
    select(-"NA") %>%
    # if there is no opposition, then we force a 0 (= opposition is not allowed to run during elections)
    mutate(opposition_adj = case_when(is.na(opposition) ~ 0, TRUE ~ opposition),
        psdi = 1-(government + opposition_adj),
        # rescale psdi to 0-1
        democracy = scales::rescale(psdi, to = c(0,1)),
        # capturing one-party systems
        democracy_adj = case_when(
            v2x_polyarchy > 0.5 ~ democracy,
            v2x_polyarchy <= 0.5 & opposition_adj == 0 ~ 0, 
            TRUE ~ democracy))

# flip gov and opposition
out <- psdi_data %>%
    rename(v2xpas_democracy_government = government,
        v2xpas_democracy_opposition = opposition_adj,
        v2xpas_democracy = democracy_adj) %>%
    select(country_id, country_text_id, year, v2xpas_democracy, v2xpas_democracy_government, v2xpas_democracy_opposition) %>%
    arrange(country_id, year)

stopifnot(!anyNA(out$country_id))
stopifnot(!anyNA(out$country_text_id))
stopifnot(!anyNA(out$year))
stopifnot(nrow(out) == nrow(distinct(out)))

out <- list(cy = out)
out <- list("v2xpas_democracy" = out)

# write file
write_file(out, OUTFILE, dir_create = TRUE)