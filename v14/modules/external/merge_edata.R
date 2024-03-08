suppressMessages(library(vutils))
suppressMessages(library(dplyr))
suppressMessages(library(vanalysis))

# Functions 
# ------------------------------------------------------------------------------
change_varnames <- function(x){
    colnames(x) <- gsub("u_vdem_country_year_", "", colnames(x))
    return(x)
    }
# ------------------------------------------------------------------------------

E_IN <- file.path("~","data","data_team","external_data","external_data_2023","output")
E_OUT <- file.path(Sys.getenv("ROOT_DIR"), "external")
if (!dir.exists(E_OUT)) dir.create(E_OUT)
country_unit <- load_country_unit()

# Add COWcode
EXT_DS_DIR <- file.path("~","data","data_team","external_data","external_data_2023")
t_table <- file.path(EXT_DS_DIR, "identifiers", "tt_2024-01-03.rds") %>% 
    read_file() %>% 
    select(country_id, year, COWcode = cow_code)

# Read files
e_dfs <-
    list.files(E_IN, full.names = TRUE) %>% 
    lapply(X = ., function(x) {
        df <- read_file(x)
        stopifnot(is.data.frame(df))

        df %<>% select(country_id, year, starts_with("e_"))
        stopifnot(ncol(df) == 3)
        stopifnot(nrow(df) > 0)
        
        return(df)
    }) %>%
    Reduce(
        f = function(left, right) {
            merge(left, right, by = c("country_id", "year"), all = TRUE)
        },
        x = .)

country_years <- with(e_dfs, interaction(country_id, year, sep = ":"))
country_years[duplicated(country_years)]
e_data <- e_dfs[!duplicated(country_years), ]
stopifnot(any(!duplicated(e_data)))
stopifnot(nrow(e_data) > 0)

# ------------------------------------------------------------------------------
# Read in updated data from Demscore
demscore <- read_file("~/data/data_team/vwork/internal/2024/demscore_qog_data/qog_data.rds") %>%
    rename_with(~ ifelse(!startsWith(., "u_vdem"), paste0("e_", .), .)) %>%
    change_varnames() %>%
    rename(e_regionpol = e_ht_region,
        e_vanhanen = e_van_index,
        e_wb_pop = e_wdi_pop,
        e_polity2 = e_p_polity2) %>%
    subset(year == 2022) 

# drop NA rows for all columns except country and year
e <- apply(demscore[, !grepl("^c|^y", names(demscore))], 1, function(r) all(is.na(r)))
demscore[e, !grepl("^c|^y", names(demscore))] <- NA
demscore %<>% filter(e != 'TRUE')

# ------------------------------------------------------------------------------
# linear interpolation and extrapolation for some of the external variables: e_mipopula, e_miurbpop, e_pefeliex, e_peinfmor, e_pelifeex, e_miinflat
linint_vars <- c(
    "e_mipopula",
    "e_miurbpop",
    "e_pefeliex",
    "e_pechmor",
    "e_pelifeex",
    "e_miinflat")
stopifnot(linint_vars %in% names(e_data))

e_data %<>% 
	group_by(country_id) %>%
	arrange(year) %>%
	mutate_at(vars(all_of(linint_vars)), function(col) {
		zoo::na.approx(col, method = "linear", na.rm = FALSE, rule = 1)
        }) %>%
	mutate(e_peaveduc = zoo::na.approx(e_peaveduc, method = "linear", na.rm = FALSE, rule = 1:2)) %>%
	ungroup() %>%
	mutate(e_miurbani = round(e_miurbpop / e_mipopula, 5)) %>%
	arrange(country_id, year)

names(e_data) <- gsub("6c", "6C", fixed = TRUE, names(e_data))

# ensure that we have only the cases from the country-unit table
e_data %<>% semi_join(country_unit, by = c("country_id", "year"))
stopifnot(nrow(e_data) == nrow(unique(e_data[,c("country_id", "year")])))

e_data %<>% merge(y = t_table, by = c("year", "country_id"), all.x = TRUE)
stopifnot(nrow(e_data) == nrow(unique(e_data[,c("country_id", "year")])))

# merge in updated Demscore data
vars <- intersect(names(demscore), names(e_data))
demscore <- demscore[, vars]

e_data_2022 <- e_data[e_data$year == 2022, ]

new <- merge(e_data_2022, demscore, by = c("year", "country_id"), all.x = TRUE) %>%
    select(!ends_with(".x")) %>%
    rename_with(~ gsub(".y$", "", .), ends_with(".y"))

e_data <- e_data %>%
    filter(year != 2022)

e_data <- rbind(e_data, new)
stopifnot(nrow(e_data) == nrow(unique(e_data[,c("country_id", "year")])))

write_file(e_data, file.path(E_OUT,"e_data.rds")) 