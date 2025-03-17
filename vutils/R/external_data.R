#' Pick external source
#'
#' Guesses the name of external source basing on the file name
#' 
#' @param file_name Full or relative path to the file
#' 
#' @details For now, it's the best way to identify the name of the source
#' although it is not recommended to use it in different context. This function is
#' vectorised, hence it can be used not only as a part of other functions but on its own.
#'     
#' @return Character vector depending on the length of the input.
#'
#' @examples
#' \dontrun{guess_source_by_file_name(list.files("~/Documents/edata/2021/upd", full.names = TRUE))}
#'
#' @export
guess_source_by_file_name <- function(file_name = "area") {
	file_name <- basename(file_name)
	source <- dplyr::case_when(
		# download Haber & Menaldo data twice
		# for one of them assign a slightly different name
		grepl("Haber", file_name) ~ "habmen",
		grepl("area", file_name) ~ "area",
		grepl("ddrevisited", file_name) ~ "cheibub",
		grepl("sp_dyn|life_expectancy|infant_mortality_rate|maternal_mortality", file_name) ~ "gapminder",
		grepl("EducationalIn", file_name) ~ "peedgini",
		grepl("powell_thyne", file_name) ~ "pt_coup",
		grepl("lied_v\\d+", file_name) ~ "lexical_index",
		grepl("_Data[.][csv|xlsx]", file_name) ~ "wbgi",
		grepl("Country_and_Territory_Ratings", file_name) ~ "fh",
		grepl("p5v2018", file_name) ~ "polity",
		grepl("PIPE", file_name) ~ "przeworski",
		grepl("_Compact", file_name) ~ "clio",
		grepl("uds_summary", file_name) ~ "uds",
		grepl("FinalCHAT", file_name) ~ "radio",
		grepl("National_COW", file_name) ~ "cowec",
		grepl("democracy-v\\d", file_name) ~ "boix",
		grepl("estimates_all_long_\\d{6}.rds$", file_name) ~ "gdppop",
		grepl("CPI\\d{4}", file_name) ~ "ti_cpi",
		grepl("qog", file_name) ~ "bnr",
		file_name == "wb_pop" ~ "wb_pop",
		TRUE ~ NA_character_
		)
	return(source)
}

choose_mtable <- function(data_key) {
	func_name <- switch(data_key,
		"area" = "cow",
		"boix" = "cow",
		"cheibub" = "cow",
		"cowec" = "cow",
		"gapminder" = "cow",
		"habmen" = "cow",
		"lexical_index" = "cow",
		"polity" = "cow",		
		"pt_coup" = "cow",
		"uds" = "cow",
		"bnr" = "iso",
		"clio" = "iso",
		"peedgini" = "iso",
		"ti_cpi" = "iso",
		"wbgi" = "iso",
		"wb_pop" = "iso",
		"gdppop" = "gw",
		"radio" = "all_df",
		"fh" = "all_df",
		"przeworski" = "mtable_list")
	return(func_name)
}

area <- function(file_name, mtable) {
	stopifnot(c("cshapes", "sf") %in% rownames(installed.packages(lib.loc = .libPaths()[1])))
	stopifnot(packageVersion("cshapes") == "2.0")

	df <- cshapes::cshp(useGW = FALSE, dependencies = TRUE)
	df <- dplyr::mutate(df, area = sf::st_area(df))

	df <- sf::st_drop_geometry(df) %>%
		dplyr::select(cowcode, country_name, start, end, status, owner, area) %>%
		dplyr::mutate(start_year = to_year(start), end_year = to_year(end)) %>%
		dplyr::arrange(cowcode, start)

	df_long <- lapply(seq_along(rownames(df)), function(row) {

		year <- with(df, start_year[row] : end_year[row])

		repl <- function(x, y) {rep(x, length(y))}

		out_df <- data.frame(
			ccode = with(df, repl(cowcode[row], year)),
			country_name = with(df, repl(country_name[row], year)),
			year = year,
			start_date = with(df, repl(start[row], year)),
			end_date = with(df, repl(end[row], year)),
			area = with(df, repl(area[row], year)),
			stringsAsFactors = FALSE
			)
		return(out_df)
	}) %>%
		dplyr::bind_rows() %>%
		dplyr::group_by(ccode, year) %>%
		dplyr::arrange(end_date) %>%
		dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
		dplyr::mutate(e_area = as.numeric(area)) %>%
		dplyr::ungroup()

	df_rec <- dplyr::mutate(df_long, ccode = dplyr::case_when(
		ccode == 255 & year == 1990 ~ 260,
		ccode == 901 ~ 900,
		ccode == 911 & year < 1949 ~ 910,
		ccode == 912 & year < 1949 ~ 910,
		ccode == 5518 & year < 1911 ~ 551,
		ccode == 822 & year < 1946 ~ 820,
		ccode == 821 & year < 1946 ~ 820,
		ccode == 7020 ~ 704,
		ccode == 521 & year < 1960 ~ 139,
		ccode == 5200 & year < 1960 ~ 139,
		ccode == 827 & year < 1946 ~ 830,
		ccode == 21 & dplyr::between(year, 1907, 1919) ~ 20,
		ccode == 732 & dplyr::between(year, 1945, 1947) ~ 730,
		ccode == 7506 & year > 1947 ~ 750,
		ccode == 681 & year < 1962~ 680,
		ccode == 6801 & year < 1967 ~ 680,
		ccode == 678 & year == 1990 ~ 679,
		ccode == 460 & year < 1922 ~ 461,
		ccode == 815 & year < 1954 ~ 817,
		ccode == 7351 & dplyr::between(year, 1905, 1919) ~ 740,
		TRUE ~ as.numeric(ccode)
	)) %>%
		dplyr::group_by(ccode, year) %>%
		dplyr::summarize(e_area = sum(e_area, na.rm = TRUE)) %>%
		dplyr::ungroup()

	ru_df <- lapply(c(516, 517), function(cid) {
		dplyr::filter(df_long, ccode == 515, year < 1962) %>%
		dplyr::mutate(ccode = cid, e_area = as.numeric(e_area)) %>%
		dplyr::select(ccode, year, e_area)
	}) %>% dplyr::bind_rows()

	df_rec %<>% dplyr::bind_rows(ru_df)

	palestine_df <- dplyr::filter(df_rec, ccode %in% c(665, 6511)) %>%
		dplyr::mutate(country_id = dplyr::case_when(
			ccode == 665 ~ 209,
			ccode == 6511 ~ 138,
			TRUE ~ as.numeric(ccode)
		)) %>% dplyr::select(country_id, year, dplyr::everything(), -ccode)

	area_df <- dplyr::inner_join(df_rec, mtable[, c("country_id", "ccode", "year")], by = c("ccode", "year")) %>%
		dplyr::bind_rows(palestine_df) %>%
		dplyr::mutate(e_area = round(e_area / 1000000, 3)) %>%
		dplyr::select(-ccode)

	return(area_df)
}

cheibub <- function(file_name, mtable) {
	cheibub <- read_file(file_name) %>%
		dplyr::select(ccode = cowcode, ctryname, year, e_chga_demo = democracy)
	cheib_df <- cheibub %>%
		dplyr::mutate(ccode = dplyr::case_when(
			ccode == 255 & year == 1990 ~ 260,
			ctryname == "Serbia" & is.na(ccode) ~ 345,
			TRUE ~ as.numeric(ccode)
		)) %>%
		dplyr::inner_join(mtable[,c("ccode", "year", "country_id")], by = c("ccode", "year")) %>%
		dplyr::select(country_id, year, everything(), -ctryname, -ccode)
	return(cheib_df)
}

gapminder <- function(file_name, mtable, country_unit) {
	gapmind <- read_file(file_name, header = TRUE)
	
	vname <- dplyr::case_when(
		grepl("maternal_mortality", basename(file_name)) ~ "e_pematmor",
		grepl("sp_dyn_tfrt", basename(file_name)) ~ "e_miferrat",
		grepl("life_expectancy", basename(file_name)) ~ "e_pelifeex",
		grepl("infant_mortality_rate", basename(file_name)) ~ "e_peinfmor",
		TRUE ~ NA_character_
		)
	stopifnot(!is.na(vname))

	gapmind_df <- dplyr::mutate_if(gapmind, is.logical, as.numeric) %>%
		dplyr::mutate_if(is.integer, as.numeric) %>%
		reshape2::melt(id.vars = "country", variable.name = "year", value.name = vname) %>%
		dplyr::mutate(year = as.numeric(as.character(year)))

	to_delete <- is.na(gapmind_df[, vname])
	gapm_nona <- gapmind_df[!to_delete,]

	add <- dplyr::mutate(gapm_nona, country_id = dplyr::case_when(
		grepl("Palestine", country) ~ 128,
	)) %>%
		dplyr::filter(!is.na(country_id)) %>%
		dplyr::semi_join(country_unit, by = c("country_id", "year"))

	gapm_df <- gapm_nona %>%
		dplyr::mutate(country = dplyr::case_when(
			grepl("Kyrgyz", country) ~ "Kyrgyzstan",
			grepl("Congo, D", country) ~ "Democratic Republic of the Congo",
			grepl("Congo, R", country) ~ "Congo",
			grepl("d'Ivoire", country) ~ "Ivory Coast",
			grepl("Lao$", country) ~ "Laos",
			grepl("Macedonia", country) ~ "Macedonia",
			grepl("Serbia", country) ~ "Yugoslavia",
			grepl("Slovak", country) ~ "Slovakia",
			grepl("Timor-Leste", country) ~ "East Timor",
			grepl("^United States$", country) ~ "United States of America",
			country == "Yemen" & dplyr::between(year, 1962, 1989) ~ "Yemen Arab Republic",
			country == "South Korea" & dplyr::between(year, 1788, 1948) ~ "Korea",
			TRUE ~ country
		)) %>%
		dplyr::inner_join(.,mtable[, c("country_name", "year", "country_id")], 
			by = c(c("country" = "country_name"), "year")) %>%
		dplyr::bind_rows(add) %>%
		dplyr::select(country_id, year, dplyr::everything(), -country) %>%
		dplyr::arrange(country_id, year)
	return(gapm_df)
}

peedgini <- function(file_name, mtable) {
	clio_df <- read_file(file_name, sheet = 2) %>%
		setNames(c("numeric_code", "country_name", "year", "e_peedgini")) %>%
		dplyr::arrange(numeric_code, year) %>%
		dplyr::inner_join(mtable[, c("country_id", "year", "numeric_code")], by = c("numeric_code", "year")) %>%
		dplyr::select(country_id, dplyr::everything(), -numeric_code, -country_name)
}

pt_coup <- function(file_name, mtable) {
	pothy_df <-	suppressWarnings(read.table(file_name, sep = "\t", header = TRUE,
			stringsAsFactors = FALSE)) %>%
		dplyr::select(ccode, year, dplyr::starts_with("coup")) %>%
		reshape2::melt(id.vars = c("ccode", "year"), variable.name = "coup_event", value.name = "e_pt_coup") %>%
		dplyr::group_by(ccode, year) %>%
		dplyr::arrange(e_pt_coup) %>%
		dplyr::filter(dplyr::row_number() == dplyr::n()) %>%
		dplyr::ungroup() %>%
		dplyr::arrange(ccode, year) %>%
		dplyr::select(-coup_event) %>%
		dplyr::mutate(ccode = dplyr::case_when(
			ccode == 678 & year == 1990 ~ 679,
			TRUE ~ as.numeric(ccode)
		)) %>%
		dplyr::inner_join(mtable[,c("ccode", "country_id", "year")], by = c("ccode", "year")) %>%
		dplyr::select(country_id, year, dplyr::everything()) %>%
		dplyr::distinct() %>%
		dplyr::select(-ccode)
	return(pothy_df)
}

lexical_index <- function(file_name, mtable) {
	lexind_df <- read_file(file_name) %>%
		dplyr::select(countryn, cow, year, lexical_index) %>%
		setNames(c("country_name", "ccode", "year", "e_lexical_index")) %>%
		dplyr::mutate(ccode = dplyr::case_when(
			ccode == 89 ~ 90,
			ccode == 300 ~ 305,
			ccode == 816 & year < 1900 ~ 817,
			ccode == 255 & year == 1990 ~ 260,
			TRUE ~ as.numeric(ccode)
		)) %>%
		dplyr::inner_join(mtable[,c("ccode", "year", "country_id")],
			by = c("ccode", "year")) %>%
		dplyr::select(-country_name, -ccode) %>%
		dplyr::select(country_id, dplyr::everything()) %>%
		dplyr::distinct()
	return(lexind_df)
}

wbgi <- function(file_name, mtable) {
	vars <- c("e_wbgi_cce", "e_wbgi_gee", "e_wbgi_pve", "e_wbgi_rle", "e_wbgi_rqe", "e_wbgi_vae")

	wbgi <- read_file(file_name, header = TRUE) %>%
		dplyr::filter(`Series Code` %in% c("CC.EST", "GE.EST", "PV.EST", "RL.EST", "RQ.EST", "VA.EST")) %>%
		dplyr::select(-`Series Name`)
	names(wbgi) <- trimws(names(wbgi), "both")

	wbgi <- reshape2::melt(wbgi, id.vars = c("Country Name", "Country Code", "Series Code")) %>%
		setNames(c("country_name", "iso3", "indicator", "year", "val")) %>%
		reshape2::dcast(country_name + iso3 + year ~ indicator, value.var = "val") %>%
		dplyr::mutate(year = gsub(" (\\[YR\\d+\\])", "", year),
			year = as.numeric(as.character(year)))
	names(wbgi)[4:9] <- vars
	to_delete <- is.na(wbgi[, 4:9]) %>% apply(1, all)
	wbgi_df <- wbgi[!to_delete,] %>% 
		dplyr::inner_join(mtable[, c("country_id", "iso3", "year")],
			by = c("iso3", "year")) %>%
		dplyr::select(country_id, dplyr::everything(), -country_name, -iso3) %>%
		dplyr::mutate_if(is.character, as.numeric)

	return(wbgi_df)
}

fh <- function(file_name, mtable, country_unit) {
	fh_df <- lapply(2:3, function(sh_n) {
		dirty_xls <- read_file(file_name, sheet = sh_n)

		nums <- dirty_xls[1, ] %>%
	    t() %>%
	    .[, 1] %>%
	    .[grepl("\\d", .)] %>%
	    lapply(function(x) rep(x, 3)) %>%
	    unlist %>% gsub("Aug|Nov|Dec|Jan|[.]| |\\d{4}-", "", x = .) %>%
	    gsub("1982", "1981", x = .)

		names(nums) <- NULL

	    cols <- dirty_xls[2,] %>% 
	    t() %>% 
	    as.vector() %>% 
	    .[-1] %>%
	    trimws("both") %>%
	    paste(nums, sep = "_")

	    df_colnames <- c("country_name", cols)
	  
	  	dirty_xls %<>%
	    setNames(df_colnames) %>%
	    .[-1:-2,]

	    dirty_xls <- reshape2::melt(dirty_xls,
	    	id.vars = "country_name", variable.name = "var_year",
	         value.name = "fh_score") %>%
		    tidyr::separate(var_year, c("indicator", "year"), sep = "_") %>% 
	    	dplyr::filter(fh_score != "-") %>%
	    	reshape2::dcast(country_name + year ~ indicator, value.var = "fh_score") %>%
		    dplyr::mutate_at(dplyr::vars(c("CL", "PR")), as.numeric)
	}) %>% 
	  dplyr::bind_rows() %>% 
	  setNames(c("country_name", "year", "e_fh_cl", "e_fh_pr", "e_fh_status")) %>% 
	  dplyr::mutate(e_fh_status = dplyr::case_when(
	    grepl("F ", e_fh_status) ~ 1,
	    e_fh_status == "F" ~ 1,
	    grepl("PF", e_fh_status) ~ 2,
	    grepl("NF", e_fh_status) ~ 3
	  ))

	fh_df[fh_df$country_name == "South Africa" & fh_df$year == 1972, "e_fh_cl"] <- 3
	fh_df[fh_df$country_name == "South Africa" & fh_df$year == 1972, "e_fh_pr"] <- 2
	fh_df[fh_df$country_name == "South Africa" & fh_df$year == 1972, "e_fh_status"] <- 1


	fht_df <- fh_df %>%
		dplyr::mutate(country_id = dplyr::case_when(
			grepl("Brazzaville", country_name) ~ 112,
			grepl("Kinshasa", country_name) ~ 111,
			grepl("d'Ivoire", country_name) ~ 64,
			grepl("Germany, E", country_name) ~ 137,
			grepl("Germany, W", country_name) ~ 77,
			grepl("United States", country_name) ~ 20,
			grepl("USSR", country_name) ~ 11,
			grepl("Vietnam, N", country_name) ~ 34,
			grepl("Vietnam, S", country_name) ~ 35,
			grepl("Yemen, N", country_name) ~ 14,
			grepl("Yemen, S", country_name) ~ 23,
			grepl("Yugoslavia ", country_name) ~ 198,
			grepl("^Gaza Strip$", country_name) ~ 138,
			grepl("West Bank", country_name) ~ 128,
			grepl("Palestinian", country_name) ~ 128
		)) %>%
		dplyr::filter(!is.na(country_id)) %>%
		dplyr::bind_rows({dplyr::inner_join(fh_df,
			dplyr::filter(mtable, country_id != 18), by = "country_name")}) %>%
		dplyr::mutate(year = as.numeric(year)) %>%
		dplyr::semi_join(country_unit, by = c("country_id", "year")) %>%
		dplyr::select(country_id, dplyr::everything(), -country_name) %>%
		dplyr::distinct()

		return(fht_df)
}

przeworski <- function(file_name, mtable_list) {
	com_df <- mtable_list[[match("com_df", names(mtable_list))]]
	mtable <- mtable_list[[match("mtable", names(mtable_list))]]

	przew <- read_file(file_name) %>%
		dplyr::select(country_name = PIPE_country, ccode = PIPE_cowcodes, country_number, year,
			e_coups = coups, e_legparty = legparty)
	to_delete <- is.na(przew[,4:5]) %>% apply(1, all)
	przew_nona <- przew[!to_delete,] %>%
		dplyr::mutate(id = 1:nrow(.))
	przew1 <- przew_nona %>%
		dplyr::mutate(ccode = dplyr::case_when(
			ccode == 300 & country_name == "Austria-Hungary (Austria)" ~ 305,
			grepl("Zaire", country_name) ~ 490,
			grepl("Swaziland", country_name) ~ 572,
			grepl("Brazzaville", country_name) ~ 484,
			grepl("^Cote", country_name) ~ 437,
			grepl("United States", country_name) ~ 2,
			grepl("Korea, S", country_name) ~ 732,
			grepl("Korea, N", country_name) ~ 731,
			grepl("Russian Federation", country_name) ~ 365,
			country_name == "Austria-Hungary (Hungary)" ~ 310,
			grepl("Serbia", country_name) ~ 345,
			TRUE ~ as.numeric(ccode)
		)) %>% 
		dplyr::inner_join(mtable[,c("ccode", "year", "country_id")], by = c("ccode", "year")) %>% 
		dplyr::group_by(ccode, year) %>%
		dplyr::filter(country_number == max(country_number)) %>%
		dplyr::ungroup
	 
	przew2 <- przew_nona %>%
		dplyr::mutate(country_name = dplyr::case_when(
			country_name == "Korea, South" ~ "Korea",
			grepl("Libyan", country_name) ~ "Libya",
			grepl("East Timor", country_name) ~ "Timor-Leste",
			grepl("Macedonia", country_name) ~ "North Macedonia",
			grepl("gran colombia", country_name, ignore.case = TRUE) & year < 1824 ~ "Colombia",
			TRUE ~ country_name
		)) %>%
		dplyr::filter(!id %in% przew1$id) %>%
		dplyr::inner_join(com_df, by = "country_name")

	przew_df <- dplyr::bind_rows(przew1, przew2)

	coups_df <- dplyr::select(przew_df, -e_legparty) %>%
		dplyr::filter(!is.na(e_coups)) %>%
		dplyr::arrange(country_id, year, dplyr::desc(e_coups)) %>%
		dplyr::distinct(country_id, year, e_coups, .keep_all = TRUE)

	legp_df <- dplyr::select(przew_df, -e_coups) %>%
		dplyr::filter(!is.na(e_legparty)) %>%
		dplyr::arrange(country_id, year, dplyr::desc(e_legparty)) %>%
		dplyr::distinct(country_id, year, e_legparty, .keep_all = TRUE) %>%
		dplyr::select(country_id, year, e_legparty)

	fin_przew <- dplyr::full_join(coups_df, legp_df, by = c("country_id", "year")) %>%
		dplyr::select(country_id, year, dplyr::everything(), -id, -country_name, -country_number, -ccode, -country_text_id)

}

polity <- function(full_name, mtable) {
	pol_df <- read_file(full_name) %>%
		dplyr::select(ccode, scode, country, year, polity, polcomp, democ, polity2, autoc) %>%
		setNames(c("ccode", "iso3", "country_name", "year", "e_p_polity", "e_polcomp", "e_democ",
			"e_polity2", "e_autoc"))

	polity_df <- pol_df %>%
		dplyr::mutate(ccode = dplyr::case_when(
			ccode == 255 & year == 1990 ~ 260,
			ccode == 678 & year == 1990 ~ 679,
			ccode == 89 ~ 90,
			ccode == 99 ~ 100,
			ccode == 342 ~ 345,
			grepl("Yugosla", country_name) ~ 345,
			grepl("Monteneg", country_name) ~ 341,
			grepl("Kosovo", country_name) ~ 347,
			ccode == 364 ~ 365,
			grepl("Sudan-North", country_name) ~ 625,
			grepl("South Sudan", country_name) ~ 626,
			ccode == 529 ~ 530,
			ccode == 769 ~ 770,
			ccode == 818 ~ 816,
			TRUE ~ as.numeric(ccode)	
		)) %>%
		dplyr::inner_join(mtable[,c("ccode", "year", "country_id")], by = c("ccode", "year")) %>%
		dplyr::arrange(country_id, year) %>%
		dplyr::select(country_id, everything(), -ccode, -iso3, -country_name) %>%
		dplyr::arrange(country_id, year) %>%
		dplyr::distinct(country_id, year, .keep_all = TRUE)
	return(polity_df)
}

clio <- function(full_name, mtable) {
	clio <- read_file(full_name, sheet = 2)
	vname <- dplyr::case_when(grepl("Internal", basename(full_name)) ~ "e_miinterc",
		grepl("International", basename(full_name)) ~ "e_miinteco",
		grepl("AverageYearsofEducation", basename(full_name)) ~ "e_peaveduc",
		grepl("FemalelifeexpectancyatBirth", basename(full_name)) ~ "e_pefeliex",
		grepl("Inflation", basename(full_name)) ~ "e_miinflat",
		grepl("TotalPopulation", basename(full_name)) ~ "e_mipopula",
		grepl("TotalUrbanPopulation", basename(full_name)) ~ "e_miurbpop",
		TRUE ~ NA_character_
		)

	clio_df <- setNames(clio, c(c("numeric_code", "country_name", "year"), vname)) %>%
		dplyr::arrange(numeric_code, year) %>%
		dplyr::inner_join(mtable[, c("country_id", "year", "numeric_code")], by = c("numeric_code", "year")) %>%
		dplyr::select(country_id, year, dplyr::everything(), -numeric_code, -country_name)

	return(clio_df)
}

uds <- function(full_name, mtable) {
	pemst_df <- read_file(full_name) %>%
		dplyr::select(ccode = cowcode, country_name = country, year,
		e_uds_mean = mean, e_uds = median, e_uds_pct025 = pct025,
		e_uds_pct975 = pct975) %>%
		dplyr::mutate(ccode = dplyr::case_when(
		ccode == 260 & year > 1990 ~ 255,
		grepl("Yemen North", country_name) & year > 1989 ~ 679,
		TRUE ~ as.numeric(ccode)
	)) %>%
	dplyr::inner_join(mtable[,c("ccode", "year", "country_id")], by = c("ccode", "year")) %>%
	dplyr::select(country_id, year, dplyr::everything(), -country_name, -ccode)
	return(pemst_df)
}

radio <- function(full_name, mtable) {
	comhob_df <- read_file(full_name, header = TRUE) %>%
	dplyr::select(country_name, year, e_radio_n = radio) %>%
	dplyr::filter(!is.na(e_radio_n)) %>%
	dplyr::mutate(country_name = dplyr::case_when(
		country_name == "Burma" ~ "Burma/Myanmar",
		grepl("Slovak ", country_name) ~ "Slovakia",
		grepl("South Vietnam", country_name) ~ "Republic of Vietnam",
		grepl("United States", country_name) ~ "United States of America",
		grepl("Venezuala", country_name) ~ "Venezuela",
		TRUE ~ country_name
	)) %>%
	dplyr::inner_join(dplyr::distinct(mtable[,c("country_id", "country_name")]), by = "country_name") %>%
	dplyr::select(country_id, year, dplyr::everything(), -country_name)
	return(comhob_df)
}

habmen <- function(full_name, mtable) {
	habmen <- read_file(full_name, sheet = 2) %>%
		dplyr::select(hmccode, cnamehabmen, year, Civil_War, Total_Fuel_Income_PC, Total_Oil_Income_PC,
			Total_Resources_Income_PC) %>%
		setNames(c("ccode", "cname", "year", "e_civil_war", "e_total_fuel_income_pc",
			"e_total_oil_income_pc", "e_total_resources_income_pc"))
	to_delete <- is.na(habmen[,4:7]) %>% apply(1, all)
	habmen_nona <- habmen[!to_delete,]
	habmen_df <- habmen_nona %>%
		dplyr::mutate(ccode = dplyr::case_when(
			(ccode == 255 & year > 1948) & (ccode == 255 & year < 1991) ~ 260,
			ccode %in% c(342, 347) ~ 345,
			ccode == 679 & year < 1990 ~ 678,
			ccode == 818 ~ 816,	
			TRUE ~ as.numeric(ccode)
		)) %>%
		dplyr::inner_join(mtable[, c("ccode", "country_id", "year")], by = c("ccode", "year")) %>%
		dplyr::select(country_id, year, dplyr::everything(), -ccode, -cname)
	return(habmen_df)
}

cowec <- function(full_name, mtable) {
	cow_econom <- read_file(full_name) %>%
		dplyr::select(ccode, statename, year, imports, exports) %>%
		setNames(c("ccode", "country_name", "year", "e_cow_imports", "e_cow_exports"))

	to_delete <- is.na(cow_econom[,4:5]) %>% apply(1, all)
	cowec_nona <- cow_econom[!to_delete,] 
	cowec_df <- cowec_nona %>%
		dplyr::mutate(ccode = dplyr::case_when(
			ccode == 300 ~ 305,
			(ccode == 255 & year > 1959) & (ccode == 255 & year < 1991) ~ 260,
			TRUE ~ as.numeric(ccode)
		)) %>%
		dplyr::inner_join(mtable[, c("ccode", "year", "country_id")], by = c("ccode", "year")) %>%
		dplyr::select(country_id, year, dplyr::everything(), -ccode, -country_name)
	return(cowec_df)
}

boix <- function(full_name, mtable) {
	boix <- read_file(full_name) %>%
		dplyr::select(country_name = country, ccode, iso3 = abbreviation, year,
			e_boix_regime = democracy,
	                e_democracy_breakdowns = democracy_breakdowns, 
	                e_democracy_omitteddata = democracy_omitteddata, 
	                e_democracy_trans = democracy_trans)

	to_delete <- is.na(boix[,5:8]) %>% apply(1, all)
	boix_nona <- boix[!to_delete,]

	boix_df <- boix_nona %>%
		dplyr::mutate(ccode = dplyr::case_when(
			ccode == 89 ~ 90,
			ccode == 347 & dplyr::between(year, 1992, 2005) ~ 345,
			ccode == 99 ~ 100,
			ccode == 347 & year < 2006 ~ 341,
			ccode == 529 ~ 530,
			ccode == 99 ~ 100,
			ccode == 769 ~ 770,
			(ccode == 342 & year != 1991) | (ccode == 342 & year != 2006) ~ 345,
			ccode == 364 ~ 365,
			ccode == 818 ~ 816,
			TRUE ~ as.numeric(ccode)
		)) %>%
		dplyr::inner_join(mtable[,c("ccode", "year", "country_id")], by = c("ccode", "year")) %>%
		dplyr::select(country_id, year, dplyr::everything(), -country_name, -ccode, -iso3) %>%
		dplyr::arrange(country_id, year)

	return(boix_df)
}

gdppop <- function(full_name, mtable, country_unit) {
	gdppop_df <- read_file(full_name)

		stopifnot(c("indicator", "mean", "sd") %in% names(gdppop_df))
		dt <- dplyr::select(gdppop_df, gwno, year, indicator, mean, sd) %>%
			dplyr::filter(grepl("latent", indicator)) %>%
			dplyr::mutate(indicator = gsub("latent_", "", indicator)) %>%
			data.table::as.data.table()

		dt <- data.table::dcast(dt, gwno + year ~ indicator,
			value.var = c("mean", "sd"))

		colnames(dt) <- gsub("(mean_|sd_)(.*)", "\\2_\\1", names(dt)) %>%
			gsub("^_|mean_", "", x = .) %>%
			gsub("_$", "", x = .) %>%
			gsub("(gdp|pop)", "e_\\1", x = .)

		mtable <- dplyr::select(mtable, country_id, year, gwno = ccode)

		gw_cnames <- dplyr::distinct(mtable, gwno, country_id)
		
		fariss_dt <- dplyr::left_join(dt, gw_cnames, by = "gwno") %>%
			dplyr::semi_join(country_unit, by = c("country_id", "year"))
	
		dups <- dplyr::group_by(fariss_dt, country_id,year) %>% 
			dplyr::tally(sort = TRUE) %>% 
			dplyr::filter(n > 1) %>% 
			dplyr::select(-n)

		non_dups <- dplyr::anti_join(fariss_dt, dups)

		dup_clean <- dplyr::semi_join(fariss_dt, dups) %>% 
			dplyr::semi_join(mtable, by = c("gwno", "year"))

		fin_gdppop_df <- dplyr::bind_rows(non_dups, dup_clean) %>%
			dplyr::select(country_id, year, dplyr::matches("^e_gdp"),
				dplyr::matches("^e_gdpp"), dplyr::matches("^e_pop")) %>%
			dplyr::arrange(country_id, year) %>%
			dplyr::mutate(dplyr::across(dplyr::starts_with("e_"),
				~round(.x, digits = 5))) %>%
			as.data.frame()

	return(fin_gdppop_df)
}

bnr <- function(full_name, mtable) {

	bnr_df <- read_file(full_name, header = TRUE) %>%
		select(numeric_code = ccode, year, bnr_dem) %>%
		mutate(numeric_code = case_when(
			numeric_code == 991 ~ 250,
		numeric_code == 992 ~ 458,
		numeric_code == 736 ~ 729,
		numeric_code == 200 ~ 203,
		numeric_code == 280 ~ 276,
		TRUE ~ as.numeric(numeric_code)
		)) %>%
		dplyr::inner_join(mtable[, c("numeric_code", "country_id", "year")],
			by = c("numeric_code", "year")) %>%
		dplyr::select(country_id, year, e_bnr_dem = bnr_dem)

	return(bnr_df)
}

ti_cpi <- function(full_name, mtable) {
	ti <- read_file(full_name, sheet = 2)
	colnames(ti) <- ti[1,]
	colnames(ti)[grepl("Country", colnames(ti))] <- "country_name"
	ti <- ti[-1,] %>%
		dplyr::select(country_name, iso3 = ISO3, dplyr::matches("CPI")) %>%
		reshape2::melt(id.vars = c("country_name", "iso3"),
			variable.name = "year", value.name = "e_ti_cpi"
		) %>%
		dplyr::mutate(year = gsub("cpi score ", "", tolower(year))) %>%
			dplyr::mutate_at(vars(c("year", "e_ti_cpi")), as.numeric)

	ti_df <- ti %>%
		mutate(iso3 = case_when(
			iso3 == "KSV" ~ "XKX",
			TRUE ~ iso3
		)) %>%
		inner_join(mtable[,c("iso3", "country_id", "year")], by = c("iso3", "year")) %>%
		select(country_id, everything(), -country_name, -iso3) %>%
		arrange(country_id, year)
	return(ti_df)
}

wb_pop <- function(full_name, mtable) {
	stopifnot("WDI" %in% rownames(installed.packages(lib.loc = .libPaths()[1])))

	wdi <- WDI::WDI(country= "all", indicator = "SP.POP.TOTL") %>%
		setNames(c("iso2", "country_name", "e_" %^% full_name, "year"))

	wdi_df <- dplyr::inner_join(wdi, mtable[,c("iso2", "country_id", "year")], by = c("iso2", "year")) %>%
		dplyr::filter(!is.na(e_wb_pop)) %>%
		dplyr::arrange(country_id, year) %>%
		dplyr::select(country_id, year, dplyr::everything(), -iso2, -country_name)

	return(wdi_df)
}

#' Load and clean external data
#'
#' Read in the data, clean it and assign V-Dem country_id
#' 
#' @param file_name Full or relative path to the file
#' 
#' @param con PostgreSQL connection to the database with external data
#' 
#' @details Nothing more, nothing less than just reading the data, guessing the source,
#' and matching cleaning function and merging table for the source. It is not
#' vectorised. Before using make sure that your .pgpass file exists and has all
#' relevant fields (the function connects to V-Dem data database to extract information
#' about countries and country units).
#'     
#' @return data.frame with V-Dem country_id, year, and target variable(s).
#'
#' @examples
#' \dontrun{load_ext_source(list.files("~/Documents/edata/2021/upd/", full.names = TRUE)[1])}
#'
#' @export
load_ext_source <- function(file_name = "wb_pop", con) {
	data_key <- guess_source_by_file_name(file_name)
	stopifnot(length(data_key) == 1,
		!is.na(data_key))

	tbl_name <- choose_mtable(data_key)

	if (tbl_name %in% c("all_df") | data_key %in% c("fh", "gapminder", "gdppop")) {

		if (nchar(Sys.getenv("ROOT_DIR")) < 1) {stop("Set ROOT_DIR environment variable!")}
		
		cu <- load_country_unit() %>%
			dplyr::select(country_id, year)
	}

	if (tbl_name %in% c("all_df")) {
		all_mtables <- lapply(c("iso", "cow", "gw"), function(tbl) {
			DBI::dbGetQuery(con, paste0("select * from mtable.", tbl, ";")) %>%
				dplyr::select(country_id, country_name)
			}) %>%
		dplyr::bind_rows()

		# I don't really like this part but let's just hope that it works...
		# don't need to invoke it too often luckily
		cntr <- load_country() %>%
			dplyr::select(country_id, country_name = name)
		
		mtable <- dplyr::bind_rows(all_mtables, cntr) %>%
			dplyr::distinct() %>%
			dplyr::semi_join(cu, by = "country_id")

		if (tbl_name == "mtable_list") {
			mtable_list <- list(com_df = mtable,
				mtable = DBI::dbGetQuery(con, "select * from mtable.cow;"))
		}

		} else {
			mtable <- DBI::dbGetQuery(con, paste0("select * from mtable.", tbl_name, ";"))
		}

	func_name <- get(data_key)
	f <- function(...) {
		out <- tryCatch(
			{message(paste("Processing source:", data_key))
			func_name(...)},
		error = function(cond) {
			message(paste("Source error:", data_key))
			message(cond)
			return(cond)
		})
		return(out)
	}
	
	if (data_key %in% c("fh", "gapminder", "gdppop")) {
			res <- f(file_name, mtable, cu)
		} else if (data_key == "przeworski") {
			res <- f(file_name, mtable_list)
			} else {
			res <- f(file_name, mtable)
		}

	return(res)
}
