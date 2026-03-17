#' Face validity check plots
#'
#' Generates face validity check plots, comparing two different versions of the dataset
#' Can be used for C, D, A, and E variables as long as they are not text data. 
#'
#' @export
create_country_plot <- function(country_data, var, min_y, max_y) {
    country <- unique(country_data$country_name)
    codelow <- names(country_data)[grepl("codelow", names(country_data))]
    codehigh <- names(country_data)[grepl("codehigh", names(country_data))]
    vold <- unique(country_data$version)[2]
    vnew <- unique(country_data$version)[1]
    min_year <- min(country_data$year)
    max_year <- as.numeric(Sys.getenv("NEWEST_YEAR"))
    year_bin_width <- 10

    if (max_year - min_year < 15) {
        year_bin_width <- 1
    }

    if (length(codelow) > 0) {
    compare_plot <- ggplot(country_data, aes(x = year, y = .data[[var]], color = version)) +
        geom_point(data = subset(country_data, version == vold), size = 1.25, alpha = 0.8) +
        geom_linerange(data = subset(country_data, version == vold), 
            aes(ymin = .data[[codelow]], ymax = .data[[codehigh]]), linewidth = 0.6, alpha = 0.8) +
        geom_point(data = subset(country_data, version == vold),
            aes(y = .data[[codelow]]), shape = 95, size = 3, stroke = 0.5) +
        geom_point(data = subset(country_data, version == vold),
            aes(y = .data[[codehigh]]), shape = 95, size = 3, stroke = 0.5) +
        # Plot vnew on top (to appear more visually dominate)
        geom_point(data = subset(country_data, version == vnew),size = 1.25, alpha = 0.8) +
        geom_linerange(data = subset(country_data, version == vnew),
            aes(ymin = .data[[codelow]], ymax = .data[[codehigh]]), linewidth = 0.6, alpha = 0.8) +
        geom_point(data = subset(country_data, version == vnew),
            aes(y = .data[[codelow]]), shape = 95, size = 3, stroke = 0.5) +
        geom_point(data = subset(country_data, version == vnew),
            aes(y = .data[[codehigh]]), shape = 95, size = 3, stroke = 0.5) +
        labs(title = sprintf("%s (%s)", var, country), x = "Year", y = var, color = "Version") +
        guides(color = guide_legend(
            override.aes = list(size = 2, shape = 16, linetype = 0))) +
        scale_color_manual(values = c("#D55E00", "#56B4E9")) +
        scale_x_continuous(breaks = seq((min_year+1), max_year, by = year_bin_width), limits = c(min_year, max_year), expand = c(0, 1)) +
        scale_y_continuous(limits = c(min_y, max_y), expand = c(0, 0.1)) +
        theme_gray() +
        theme(plot.title = element_text(size = 20),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            panel.grid.major.y = element_line(color = "white", linewidth = 0.5),
            panel.grid.minor.y = element_line(linetype = "dashed", color = "white", linewidth = 0.5),
            panel.grid.minor.x = element_line(linetype = "dashed", color = "white", linewidth = 0.5),
            panel.grid.major.x = element_line(color = "white", linewidth = 0.5))
    } else if (length(codelow) == 0) {
        compare_plot <- ggplot(country_data, aes(x = year, y = .data[[var]], color = version)) +
            geom_point(data = subset(country_data, version == vold), size = 1.25, alpha = 0.8) +
            # Plot vnew on top (to appear more visually dominate)
            geom_point(data = subset(country_data, version == vnew),size = 1.25, alpha = 0.7) +
            labs(title = sprintf("%s (%s)", var, country), x = "Year", y = var, color = "Version") +
            guides(color = guide_legend(
                override.aes = list(size = 2, shape = 16, linetype = 0))) +
            scale_color_manual(values = c("#D55E00", "#56B4E9")) +
            scale_x_continuous(breaks = seq((min_year+1), max_year, by = year_bin_width), limits = c(min_year, max_year), expand = c(0, 1)) +
            scale_y_continuous(limits = c(min_y, max_y), expand = c(0, 0.1)) +
            theme_gray()  +
            theme(plot.title = element_text(size = 20),
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 16),
                axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
                axis.text.y = element_text(size = 14),
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16),
                panel.grid.major.y = element_line(color = "white", linewidth = 0.5),
                panel.grid.minor.y = element_line(linetype = "dashed", color = "white", linewidth = 0.5),
                panel.grid.minor.x = element_line(linetype = "dashed", color = "white", linewidth = 0.5),
                panel.grid.major.x = element_line(color = "white", linewidth = 0.5))
    }
    return(compare_plot)
}

#' Creates components plots for D variables
#' Showing all components of the index (and rescaling where needed)
#' @export
create_component_plot <- function(components_df, component_vartype, index, qtable) {
    country <- unique(components_df$country_name)
    min_year <- min(components_df$year)
    max_year <- as.numeric(Sys.getenv("NEWEST_YEAR"))

    year_bin_width <- 10
    if (max_year - min_year < 15) {
        year_bin_width <- 1
    }

    if (all(component_vartype$vartype == "D")) {
        min_y = 0
        max_y = 1
    } else if (any(component_vartype$vartype == "C")) {
        cvars <- gsub("_osp", "", component_vartype[component_vartype$vartype == "C", ]$name)
        cscale <- qtable[qtable$name %in% cvars, c("name", "k")]

        min_y = 0
        max_y = max(cscale$k) - 1

        # rescaling so all c vars are the same
        if (!all(cscale$k == (max_y +1))) {
            rescale_var <- cscale[cscale$k != (max_y + 1), ]$name

            for (var in rescale_var) {
                info(sprintf("Rescaling %s", var))
                cur_max <- cscale[cscale$name == var, "k"] - 1

                var_osp <- names(components_df)[grepl(var, names(components_df))]

                components_df[[var_osp]] <- (components_df[[var_osp]]) / (cur_max) * (max_y)
                names(components_df)[names(components_df) == var_osp] <- gsub("_osp", "_adj", var_osp)

            }
        }
    } else if (all(component_vartype$vartype == "A")) {
        avars <- component_vartype[component_vartype$vartype == "A", ]$name

        min_y = 0
        max_y = max(components_df[avars], na.rm = TRUE)
    } else {
        min_y = 0
        max_y = 1
    }

    # rescaling D vars if in components with C vars
    if (all(c("D", "C") %in% component_vartype$vartype)) {
        dvar <- component_vartype[component_vartype$vartype == "D", ]$name

        for (var in dvar) {
            info(sprintf("Rescaling %s", var))

            components_df[[var]] <- (components_df[[var]]) / (1) * (max_y)
            names(components_df)[names(components_df) == var] <- paste0(var, "_adj")
        }
    }

    # rescaling A vars if in components with C or D vars
    if (any(grepl("A", component_vartype$vartype))) {
        avar <- component_vartype[grepl("A", component_vartype$vartype), ]$name

        for (var in avar) {
            info(sprintf("Rescaling %s", var))

            components_df[[var]] <- (components_df[[var]]) / (max(components_df[[var]], na.rm = TRUE)) * (max_y)
            names(components_df)[names(components_df) == var] <- paste0(var, "_adj")
        }
    }

    pivot_df <- components_df %>%
        wide_to_long(id_vars = c("country_id", "country_text_id", "country_name", "year")) %>%
        mutate(group = cumsum(c(1, diff(year) != 1))) %>%
        filter(!is.na(value)) 

    okabe_ito <- c("#D55E00", "#56B4E9", "#CC79A7","#009E73", "#F0E442", "#E69F00", "#0072B2", "#000000", "#999999","#FF0000")

    compare_plot <- ggplot(pivot_df, aes(x = year, y = value, color = variable, group = interaction(variable, group))) +
        geom_line() +
        labs(title = sprintf("%s components (%s)", index, country), x = "Year", y = "point_estimates", color = "Variable") +
        scale_x_continuous(breaks = seq((min_year+1), max_year, by = year_bin_width), 
            limits = c(min_year, max_year), expand = c(0, 1)) +
        scale_y_continuous(limits = c(min_y, max_y), expand = c(0, 0.1)) +
        scale_color_manual(values = okabe_ito) +
        theme_gray() +
        theme(panel.grid.major.y = element_line(color = "white", linewidth = 0.5),
            panel.grid.minor.y = element_line(linetype = "dashed", color = "white", linewidth = 0.5),
            panel.grid.minor.x = element_line(linetype = "dashed", color = "white", linewidth = 0.5),
            panel.grid.major.x = element_line(color = "white", linewidth = 0.5),
            plot.title = element_text(size = 20),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
            axis.text.y = element_text(size = 14),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16))

    return(compare_plot)
}
