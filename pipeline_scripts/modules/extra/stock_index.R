#!/usr/bin/env Rscript

# --------------------------------------------------------------------------
# The following script generates stock versions of the HLIs based on the code in the demstock package's get_stock() function
# These functions have been re-written to fit into the pipeline.
# See demstock package for more info / contact Matt, Amanda, or Vanessa for more info.
# --------------------------------------------------------------------------

suppressMessages(library(dplyr))
suppressMessages(library(vutils))
suppressMessages(library(vpipe))

# Functions
# --------------------------------------------------------------------------
# Subset variables
remove_hist_countries <- function(df) {
	`%!in%` <- Negate(`%in%`)

    # removes German and Italian historical states
	df <- filter(df, country_id %!in% c(349,350,351,352,353,354,355,356,357,358,359,360,361,362,363,364,365,366,373))

	info("Removed German and Italian historical states")
    return(df)
}

# Expand data
expand_data <- function(df, DEP_TASK) {
    df_out <- df %>%
        select(country_id, country_text_id, year, all_of(DEP_TASK)) %>%
        mutate(original = 1)

    min_year <- min(df_out$year, na.rm = TRUE)
    max_year <- max(df_out$year, na.rm = TRUE)

    # create df with all possible country_id, year combinations
    fullset <- df_out %>%
        distinct(country_id, country_text_id) %>%
        rowwise() %>%
        mutate(year = list(seq(min_year, max_year))) %>%
        tidyr::unnest(year) %>%
        select(country_id, country_text_id, year) %>%
        as.data.frame() 
    
    # merge fullset and df to expand all possible combinations
    df_out <- merge(df_out, fullset, all = TRUE) %>%
        arrange(country_id, year) %>%
        mutate(original = ifelse(is.na(original), 0, original)) 
    rownames(df_out) <- NULL

    stopifnot(nrow(df_out) == nrow(fullset))
    stopifnot(!anyNA(df_out$country_id))
    stopifnot(!anyNA(df_out$country_text_id))
    stopifnot(unique(filter(df_out, original == 0)[[DEP_TASK]]) %>% is.na()) 

    df_out <- select(df_out, country_id, country_text_id, year, all_of(DEP_TASK))

    info(sprintf("Subset data expanded. Old dimensions were %d by %d. New dimensions are %d by %d", 
        dim(df)[1], dim(df)[2]-1, dim(df_out)[1], dim(df_out)[2]))

    return(df_out)
}

# Create stock_id according to the stock translation table
create_stock_id <- function(df, stock_tt) {

    df_out <- left_join(df, stock_tt, by = c("country_id", "country_text_id", "year"))

    stopifnot(nrow(df_out) == nrow(df))
    stopifnot(nrow(filter(df_out, !is.na(stock_id))) == nrow(stock_tt))
    stopifnot(!anyNA(df_out$country_id))
    stopifnot(!anyNA(df_out$country_text_id))
    stopifnot(!anyNA(df_out$year))    

    df_out <- df_out %>%
        mutate(stock_id = case_when(is.na(stock_id) ~ country_id,
            TRUE ~ stock_id))

    stopifnot(!anyNA(df_out$stock_id))

    info("Stock country ID created.")
    return(df_out)
}

# Create measure based on historical antecedence
create_antecedence <- function(df, TASK_NAME, DEP_TASK, stock_tt){
    # create new stock var
    df_out <- df
    df_out[[TASK_NAME]] <- df_out[[DEP_TASK]]

    temp <- subset(df_out, stock_id %in% unique(stock_tt$stock_id))

    stopifnot(length(unique(temp$stock_id)) == length(unique(stock_tt$stock_id)))

    temp <- temp %>% 
        group_by(stock_id, year) %>% 
            mutate("tempmean" := mean(!!as.name(TASK_NAME), na.rm=T)) %>%
        ungroup() %>%
        select(stock_id, country_id, year, tempmean) %>%
        # change NaN to NA
        mutate(tempmean = ifelse(is.nan(tempmean), NA, tempmean))

    # replace normal values with tempmean where country_id != stock_id
    df_out <- df_out %>%
        left_join(temp, by = c("stock_id", "country_id", "year")) %>% 
        mutate(!!as.name(TASK_NAME) := ifelse(country_id != stock_id, tempmean, !!as.name(TASK_NAME))) %>%
        select(-tempmean) %>%
        arrange(country_id, year) %>%
        mutate(!!as.name(TASK_NAME) := case_when(
            # Carrying forward Vietnam, which shows similar values before and after a gap from 1887-1902. This was all French colonial rule period 
            country_id == 34 & year >= 1888 & year <= 1901 ~ locf(!!as.name(TASK_NAME)),
            # # Carrying forward Egypt, which shows similar values before and after a gap from 1883-1912. This was all British colonial rule period
            TRUE ~ !!as.name(TASK_NAME))) 
    # add averaged values from Mali, Niger, and Ivory Coast to Burkina Faso 1932-1942 under French West Africa
    avg_fwa <- df_out %>%
        filter(year >= 1932 & year <= 1946 & country_id %in% c(64, 28, 60)) %>%
        group_by(year) %>%
            summarize(avg_value = mean(!!as.name(TASK_NAME), na.rm = TRUE)) %>% 
        mutate(country_id = 54)

    df_out <- df_out %>%
        left_join(avg_fwa, by = c("country_id", "year")) %>%
        mutate(!!as.name(TASK_NAME) := case_when(
            country_id == 54 & year >= 1932 & year <= 1946 ~ avg_value,
            TRUE ~ !!as.name(TASK_NAME))) %>%
        select(-avg_value) 
	
    stopifnot(nrow(df_out) == nrow(df))
    stopifnot(!anyNA(df_out$country_id))
    stopifnot(!anyNA(df_out$country_text_id))
    stopifnot(!anyNA(df_out$year))

    info("Antecedent values imputed.")
    return(df_out)
}

# Normalize and fill forward years
fill_data <- function(df, TASK_NAME, fill) {
    stopifnot(fill > 0)
    stopifnot(any(grepl(TASK_NAME, names(df))))
    stopifnot(is.numeric(df[[TASK_NAME]]))

    df_out <- df %>%
        mutate(norm_value = !!as.name(TASK_NAME)) %>%
        mutate(filled = norm_value)

    # Fill forward
    for (lagval in c(1:fill)) {
        df_out <- df_out %>% 
            group_by(country_id) %>% 
                mutate(!!as.name(paste0("L",lagval)) := lag(norm_value, n=lagval)) %>%
            ungroup() %>%
            mutate(filled = case_when(is.na(filled) ~ !!as.name(paste0("L",lagval)),
                TRUE ~ filled)) %>%
            select(-!!as.name(paste0("L",lagval)))
    }

    stopifnot(nrow(filter(df_out, is.na(norm_value) & !is.na(filled))) == 
        nrow(filter(df_out, is.na(norm_value))) - nrow(filter(df_out, is.na(filled))))
    info(sprintf("%s values filled.", nrow(filter(df_out, is.na(norm_value) & !is.na(filled)))))

    df_out <- select(df_out, -c(norm_value, as.name(TASK_NAME))) %>%
        rename(!!TASK_NAME := filled)

    info(paste0("Variables filled forward ", fill, " years."))
    return(df_out)
}

# Calculate stock
calc_stock <- function(df, TASK_NAME, DEP_TASK, val) {
    stopifnot(is.numeric(val))

    info(sprintf("Calculating stock for %s with depreciation rate of %s%%", DEP_TASK, 100*(1-val)))

	df_out <- df %>%
        mutate(startyear = case_when(!is.na(!!as.name(TASK_NAME)) ~ year, TRUE ~ NA)) %>%
        group_by(country_id) %>%
            mutate(startyear = min(startyear, na.rm = TRUE)) %>%
        ungroup() %>%
        #sets values to zero for first observed year
        mutate(val_version = case_when(year == startyear ~ 0, TRUE ~ NA)) %>%
        as.data.frame()

    # calculate stock
    pb <- txtProgressBar(min = 0, max = max(as.numeric(df_out$year) - as.numeric(unlist(df_out[,which(colnames(df_out) == "val_version")])), na.rm = TRUE), style = 3)

    for (y in 1:max(as.numeric(df_out$year)-as.numeric(unlist(df_out[,which(colnames(df_out) == "startyear")])), na.rm = TRUE)) {
        setTxtProgressBar(pb, value=y)

        df_out <- df_out %>% 
            group_by(country_id) %>% 
            mutate(Lval = lag(val_version)) %>%
            ungroup()

        df_out <- df_out %>% 
            group_by(country_id) %>% 
            mutate(Lfilled = lag(!!as.name(TASK_NAME))) %>%
            ungroup()

        # sets all other years to previous year's stock times depreciation, plus previous year's democracy level
        df_out[which(is.na(df_out[,which(colnames(df_out) == "val_version")]) == TRUE & 
            (as.numeric(df_out$year) - as.numeric(unlist(df_out[,which(colnames(df_out) == "startyear")]))) == y), 
            which(colnames(df_out) == "val_version")] <- (val*df_out[which(is.na(df_out[,which(colnames(df_out) == "val_version")]) == TRUE & 
            (as.numeric(df_out$year) - as.numeric(unlist(df_out[,which(colnames(df_out) == "startyear")]))) == y),]$Lval)+
            df_out[which(is.na(df_out[,which(colnames(df_out) == "val_version")]) == TRUE & 
            (as.numeric(df_out$year)-as.numeric(unlist(df_out[,which(colnames(df_out) == "startyear")]))) == y),]$Lfilled
    }
    
    close(pb)

    df_out <- df_out %>%
        # change first year back to NA
        mutate(val_version = case_when(year == startyear ~ NA, TRUE ~ val_version)) %>%     
        # drop stock values for antecedent years
        mutate(og_year = case_when(!is.na(!!as.name(DEP_TASK)) ~ year, TRUE ~ NA)) %>%
        group_by(country_id) %>%
            mutate(keepyear = min(og_year, na.rm = TRUE)) %>%
        ungroup() %>%
        select(-og_year) %>% 
        mutate(val_version = case_when(year < keepyear ~ NA, TRUE ~ val_version)) %>%
        select(-keepyear) %>%
        mutate(!!TASK_NAME := val_version*(1 - val)) %>%
        select(-c(Lval, Lfilled, startyear, val_version, as.name(DEP_TASK))) %>%
        as.data.frame()

    info(sprintf("%s calculated.", TASK_NAME)) 

    return(df_out)
}

# Main
main <- function(df, TASK_NAME, DEP_TASK, val, fill) {
    # Prepare data
    df <- remove_hist_countries(df)
    # until PM fix historical states remove from stock_tt
    stock_tt <- remove_hist_countries(stock_tt)
    df <- expand_data(df, DEP_TASK)

    df <- create_stock_id(df, stock_tt)
    df <- create_antecedence(df, TASK_NAME, DEP_TASK, stock_tt)
    df <- fill_data(df, TASK_NAME, fill = 5)

    # Calculate stock
    df <- calc_stock(df, TASK_NAME, DEP_TASK, val = 0.99)

    df <- df %>%
        select(country_id, country_text_id, year, all_of(TASK_NAME))

    return(list(
        cy = df))
}


# Run task
# --------------------------------------------------------------------------
db <- pg_connect()
get_globals()

ctable <- load_country()
ctable <- ctable[!grepl("\\*", ctable$name),]

DEP_TASK <- gsub("_stock", "", TASK_NAME)
objs <- find_dep_files(TASK_ID, db)[[DEP_TASK]][[DEP_TASK]]$cy %>%
    left_join(distinct(ctable, country_text_id, country_id), by = "country_text_id")
stock_tt <- read_file("~/data/data_team/vwork/internal/2025/2025-12-10_stock_indices/stock_translation_table_stretched.csv")

stopifnot(!anyNA(objs$country_id))
stopifnot(!anyNA(objs$country_text_id))
stopifnot(!anyNA(stock_tt))

out <- main(objs, TASK_NAME, DEP_TASK, val = 0.99, fill = 5) 
out <- setNames(list(out), TASK_NAME)

write_file(out, OUTFILE, dir_create = TRUE)

info("Finished: " %^% TASK_NAME)
