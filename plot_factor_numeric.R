# ------------------------------------------------------------------------------
# FACTOR VS NUMERIC EXPLORATORY DATA ANALYSIS
# ------------------------------------------------------------------------------
plot_factor_numeric <- function(data, #  data frame or tibble (obligatory parameter)
                                factor_var, # factor variable (obligatory parameter)
                                numeric_var, # numeric variable (obligatory parameter)
                                variables_as_string = FALSE, # if FALSE factor_var_1 and factor_var_2 without "" 
                                # if TRUE factor_var_1 and factor_var_2 in ""
                                
                                # Sample data from original datatset to make faster function compilation:
                                data_size = 1.0, # dataset size - e.g 1.0 (= 100% -> all observations), e.g 0.5 (= 50% -> half of observations)
                                seed_value = 42, # seed for random data generation 
                                
                                # Axises and labels:
                                factor_axis = NULL, # name of factor variable asix
                                numeric_axis = NULL, # name of numeric variable axis
                                count_axis = "COUNT", # name of count axis
                                percentage_axis = "PERCENTAGE", # name of percentage axis
                                density_axis = "DENSITY", # name of density axis
                                caption = NULL, # caption
                                
                                # Charts titles:
                                title_1 = "BAR PLOT (N)", 
                                title_2 = "BAR PLOT (%)",
                                title_3 = "WAFFLE CHART",
                                title_4 = "CIRCLE PACKING PLOT",
                                title_5 = "BOX PLOT",
                                title_6 = "VIOLIN PLOT",
                                title_7 = "RIDGELINE PLOT",
                                title_8 = "DENSITY PLOT",
                                title_9 = "DIFFERENCE OF GROUP MEAN TO TOTAL MEAN",                                 
                                title_10 = "DIFFERENCE OF GROUP MEDIAN TO TOTAL MEDIAN",
                                title_11 = "DISTRIBUTION QUANTILE CUT PLOT",
                                title_12 = "EMPIRICAL CUMULATIVE DISTRIBUTION PLOT",
                                title_13 = "BOX PLOT & VIOLIN PLOT",
                                title_14 = "HISTOGRAM",
                                title_15 = "QUANTILE-QUANTILE PLOT",
                                title_16 = "DISTRIBUTION PLOT",
                                
                                # Additional parameters:
                                title_size = 9, # font size for title
                                text_size = 7, # font size for axises, labels, caption etc.
                                histogram_bins = 10, 
                                numeric_cuts = 10, # cuts 
                                grid_size = 50, # graph grid density in waffle charts
                                label_size = 3, # font size for labels
                                digits_lab = 2, # decimal numbers in distribution quantile cut plot
                                
                                # Plot save parameters:
                                save_plots = FALSE, # FALSE - plot is not saved; TRUE - plot is saved
                                save_filename = NULL, # filename for saved file; if NULL savefilename consists of variables namesand current datetime
                                save_width = 64, # default aspekt ratio = 4 * 16:9
                                save_height = 36, # default aspekt ratio = 4 * 16:9
                                save_dpi = 100, # plot resolution
                                save_file_format = ".png", # file save format
                                save_plots_units = "cm", # file save units
                                
                                # Grid display parameters:
                                plot_grid = base::matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), byrow = TRUE, ncol = 4)){ # plot_grid default parameterization is recommended
  
  # ------------------------------------------------------------------------------ 
  # Packages and libraries installation:
  if (!require(ggplot2)){utils::install.packages('ggplot2'); require('ggplot2')}
  if (!require(tidyverse)){utils::install.packages('tidyverse'); require('tidyverse')}
  if (!require(dplyr)){utils::install.packages('dplyr'); require('dplyr')}
  if (!require(magrittr)){utils::install.packages('magrittr'); require('magrittr')} 
  if (!require(gridExtra)){utils::install.packages('gridExtra'); require('gridExtra')}
  if (!require(scales)){utils::install.packages('scales'); require('scales')}
  if (!require(stringr)){utils::install.packages('stringr'); require('stringr')}
  if (!require(ggridges)){utils::install.packages('ggridges'); require('ggridges')}
  if (!require(e1071)){utils::install.packages('e1071'); require('e1071')}  
  if (!require(knitr)){utils::install.packages('knitr'); require('knitr')}  
  if (!require(packcircles)){utils::install.packages('packcircles'); require('packcircles')}  
  if (!require(viridis)){utils::install.packages('viridis'); require('viridis')}  
  
  # ------------------------------------------------------------------------------
  # Additional options:
  base::options(scipen = 20)
  base::options(warn = -1)
  datetime <- stringr::str_replace_all(base::Sys.time(), ":", "-")
  
  # ------------------------------------------------------------------------------
  # Variables:
  if (variables_as_string == FALSE){
    numeric_var <- dplyr::enquo(numeric_var)                    
    factor_var  <- dplyr::enquo(factor_var) 
    numeric_var_name <- dplyr::quo_name(numeric_var)
    factor_var_name <- dplyr::quo_name(factor_var)}
  
  if (variables_as_string == TRUE){
    numeric_var <- rlang::sym(numeric_var)
    factor_var <- rlang::sym(factor_var)
    numeric_var <- dplyr::enquo(numeric_var)                    
    factor_var  <- dplyr::enquo(factor_var) 
    numeric_var_name <- dplyr::quo_name(numeric_var)
    factor_var_name <- dplyr::quo_name(factor_var)}
  
  # ------------------------------------------------------------------------------
  # Conditions:
  if (base::is.data.frame(data) | tibble::is_tibble(data)){
    base::cat("INFO: Type of provided data is appropriate"); base::cat("\n")
    if (base::is.factor(data[[base::which(base::names(data) == factor_var_name)]])){
      base::cat("INFO: Type of provided factor_var is appropariate"); base::cat("\n")
      if (base::is.numeric(data[[base::which(base::names(data) == numeric_var_name)]]) | base::is.integer(data[[base::which(base::names(data) == numeric_var_name)]])){
        base::cat("INFO: Type of provided numeric_var is appropariate"); base::cat("\n")
        if (base::is.null(factor_axis)){factor_axis <- "FACTOR VARIABLE"} else {factor_axis <- stringr::str_to_upper(factor_axis)}
        if (base::is.null(numeric_axis)){numeric_axis <- "NUMERIC VARIABLE"} else {numeric_axis <- stringr::str_to_upper(numeric_axis)}
        if (base::is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(base::paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- dplyr::as_tibble(data)
        base::set.seed(seed = seed_value)
        data %>% dplyr::sample_frac(size = data_size) -> data
        
        # ------------------------------------------------------------------------------       
        # PLOT 1:
        data %>% 
          dplyr::select(!!factor_var) %>% 
          dplyr::group_by(!!factor_var) %>% 
          dplyr::summarise(count = dplyr::n(), percentage = dplyr::n()/base::nrow(.)) -> data_cut
        data %>% 
          dplyr::select(!!factor_var) -> var; var <- base::c(var); var <- base::unlist(var)
        mean_value <- base::length(var)/base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]]))
        ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var, y = count, label = count, fill = !!factor_var)) +
          ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
          ggplot2::labs(x = factor_axis, y = count_axis, title = title_1) +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") +
          ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) +
          ggplot2::scale_y_continuous(limits = base::c(0, 1.1 * base::max(data_cut$count))) -> plot1
        
        # ------------------------------------------------------------------------------
        # PLOT 2:  
        data %>% dplyr::select(!!numeric_var) -> var; var <- base::c(var); var <- base::unlist(var)
        mean_value <- (base::length(var)/base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])))/base::length(var)
        ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var, y = percentage, fill = !!factor_var, label = base::paste(100 * base::round(percentage, 4), "%", sep = ""))) +
          ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
          ggplot2::labs(x = factor_axis, y = percentage_axis, title = title_2) +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") +
          ggplot2::scale_y_continuous(limits = base::c(0, 1.1 * base::max(data_cut$percentage)), 
                                      labels = base::c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"), 
                                      breaks = base::c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
          ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot2
        
        # ------------------------------------------------------------------------------
        # PLOT 3:  
        data %>% dplyr::select(!!factor_var) -> var
        df <- base::expand.grid(y = 1:grid_size, x = 1:grid_size)
        categ_table <- base::round(table(var) * ((grid_size * grid_size)/base::nrow(var)))
        if(base::sum(categ_table) == grid_size * grid_size){
          categ_table <- categ_table
        } else if (base::sum(categ_table) > grid_size * grid_size){
          los = base::factor(base::rep(base::names(categ_table), categ_table))
          los = base::sample(los, grid_size * grid_size, replace = FALSE)
          categ_table <- base::table(los)
        } else {
          los =  grid_size * grid_size - base::sum(categ_table)
          categ_table[base::which.max(categ_table)] <- categ_table[base::which.max(categ_table)] + los
        }
        df$var <- base::factor(base::rep(base::names(categ_table), categ_table), levels = base::levels(data[[base::which(base::names(data) == factor_var_name)]]), ordered = TRUE)
        ggplot2::ggplot(df, aes(x = x, y = y, fill = var)) +
          ggplot2::geom_tile(color = "black") +
          ggplot2::scale_x_continuous(expand = base::c(0, 0)) +
          ggplot2::scale_y_continuous(expand = base::c(0, 0), trans = "reverse") + 
          ggplot2::labs(title = title_3) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                         legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                         legend.position = "right",
                         legend.box.spacing = ggplot2::unit(0.25, "cm"),
                         legend.text = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         legend.title = ggplot2::element_text(size = text_size, color = "black", face = "bold")) +
          ggplot2::guides(fill = guide_legend(base::paste(factor_axis, ":", sep = ""), ncol = 1)) +
          ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot3
        
        # ------------------------------------------------------------------------------    
        # PLOT 4:
        circle_data <- data %>%
          dplyr::select(!!factor_var) %>%
          dplyr::arrange(desc(!!factor_var)) %>%
          dplyr::group_by(!!factor_var) %>%
          dplyr::count() %>%
          dplyr::ungroup() %>%
          dplyr::mutate(prop = 100 * n/base::sum(n, na.rm = TRUE)); base::as.data.frame(circle_data) -> circle_data
        packing <- packcircles::circleProgressiveLayout(circle_data$n, sizetype='area')
        data_packing = base::cbind(circle_data, packing)
        ggplot_circle_data <- packcircles::circleLayoutVertices(packing, npoints = 10000)
        ggplot2::ggplot() + 
          ggplot2::geom_polygon(data = ggplot_circle_data, aes(x, y, group = id, fill = as.factor(id)), colour = "black") +
          ggplot2::geom_label(data = data_packing, aes(x = x, y = y, label = !!factor_var), color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
          ggplot2::scale_size_continuous(range = base::c(1,4)) +
          ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) +
          ggplot2::labs(title = title_4) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.title.x = ggplot2::element_blank(),
                         axis.text.x = ggplot2::element_blank(),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         axis.text.y = ggplot2::element_blank(),
                         axis.ticks.y = ggplot2::element_blank(),
                         panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") -> plot4
        
        # ------------------------------------------------------------------------------     
        # PLOT 5:  
        data %>% dplyr::select(!!numeric_var) -> var; var <- base::c(var); var <- base::unlist(var); mean_value <- base::mean(var, na.rm = TRUE)
        data %>% dplyr::select(!!numeric_var) -> var; var <- base::c(var); var <- base::unlist(var); median_value <- stats::median(var, na.rm = TRUE)
        data %>% dplyr::select(!!numeric_var) -> var; var <- base::c(var); var <- base::unlist(var); Q1_value <- stats::quantile(var, 0.25, na.rm = TRUE)
        data %>% dplyr::select(!!numeric_var) -> var; var <- base::c(var); var <- base::unlist(var); Q3_value <- stats::quantile(var, 0.75, na.rm = TRUE)
        ggplot2::ggplot(data = data, aes(x = !!factor_var, y = !!numeric_var, fill = !!factor_var)) +
          ggplot2::geom_hline(yintercept = mean_value, lty = 1, lwd = 0.5, color = "black") +
          ggplot2::geom_hline(yintercept = median_value, lty = 2, lwd = 0.5, color = "black") +    
          ggplot2::geom_hline(yintercept = Q1_value, lty = 2, lwd = 0.5, color = "black") +   
          ggplot2::geom_hline(yintercept = Q3_value, lty = 2, lwd = 0.5, color = "black") +   
          ggplot2::geom_boxplot(color = "black", outlier.color = "black", notch = FALSE) + 
          ggplot2::stat_summary(fun = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
          ggplot2::stat_summary(fun = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
          ggplot2::labs(x = factor_axis, y = numeric_axis, title = title_5) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") +
          ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot5
        
        # ------------------------------------------------------------------------------
        # PLOT 6:  
        data %>% dplyr::select(!!numeric_var) -> var; var <- base::c(var); var <- base::unlist(var); mean_value <- base::mean(var, na.rm = TRUE)
        data %>% dplyr::select(!!numeric_var) -> var; var <- base::c(var); var <- base::unlist(var); median_value <- stats::median(var, na.rm = TRUE)
        data %>% dplyr::select(!!numeric_var) -> var; var <- base::c(var); var <- base::unlist(var); Q1_value <- stats::quantile(var, 0.25, na.rm = TRUE)
        data %>% dplyr::select(!!numeric_var) -> var; var <- base::c(var); var <- base::unlist(var); Q3_value <- stats::quantile(var, 0.75, na.rm = TRUE)
        ggplot2::ggplot(data = data, aes(x = !!factor_var, y = !!numeric_var, fill = !!factor_var)) +
          ggplot2::geom_hline(yintercept = mean_value, lty = 1, lwd = 0.5, color = "black") +
          ggplot2::geom_hline(yintercept = median_value, lty = 2, lwd = 0.5, color = "black") +    
          ggplot2::geom_hline(yintercept = Q1_value, lty = 2, lwd = 0.5, color = "black") +   
          ggplot2::geom_hline(yintercept = Q3_value, lty = 2, lwd = 0.5, color = "black") +   
          ggplot2::geom_violin(draw_quantiles = base::c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", trim = FALSE) +
          ggplot2::stat_summary(fun = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
          ggplot2::stat_summary(fun = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
          ggplot2::labs(x = factor_axis, y = numeric_axis, title = title_6) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") +
          ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot6
        
        # ------------------------------------------------------------------------------
        # PLOT 7:
        ggplot2::ggplot(data, aes(x = !!numeric_var, y = !!factor_var, fill = !!factor_var)) +
          ggridges::geom_density_ridges2(scale = 0.95, quantile_lines = TRUE, rel_min_height = 0.001) + 
          ggplot2::labs(x = numeric_axis, y = factor_axis, title = title_7) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") +
          ggplot2::guides(fill = guide_legend(paste(factor_axis, ":", sep = ""), nrow = 1)) +
          ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) +
          ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = 0.05)) -> plot7
        
        # ------------------------------------------------------------------------------
        # PLOT 8:
        ggplot2::ggplot(data = data, aes(x = !!numeric_var)) +
          ggplot2::geom_density(aes(fill = !!factor_var), position = "stack") +
          ggplot2::labs(x = numeric_axis, y = percentage_axis, title = title_8) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.25, "cm"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                         legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                         legend.position = "right",
                         legend.box.spacing = ggplot2::unit(0.1, "cm"),
                         legend.text = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         legend.title = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         legend.key = ggplot2::element_rect(color = "gray90")) +
          ggplot2::guides(fill = guide_legend(base::paste(factor_axis, ":", sep = ""), ncol = 1)) +
          ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot8
        
        # ------------------------------------------------------------------------------  
        # PLOT 9:
        data %>%
          dplyr::mutate(mean_value = base::mean(!!numeric_var, na.rm = TRUE)) %>%
          group_by(!!factor_var) %>%
          dplyr::mutate(mean_group = base::mean(!!numeric_var, na.rm = TRUE)) %>%
          dplyr::select(!!factor_var, mean_value, mean_group) %>%
          dplyr::distinct() %>%
          dplyr::mutate(type = base::ifelse(mean_group < mean_value, "Mean below average", "Mean above average")) %>%
          dplyr::arrange(mean_group) %>%
          dplyr::mutate(mean_group = mean_group - mean_value) %>%
          dplyr::ungroup() -> data_chart
        ggplot(data = data_chart, aes(x = !!factor_var, y = mean_group, label = base::round(mean_group, 2))) +
          ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 1 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 1 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 2 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 2 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 3 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 3 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
          ggplot2::scale_fill_manual(name = "GROUP:",
                                     labels = base::c("MEAN ABOVE AVERAGE", "MEAN BELOW AVERAGE"),
                                     values = base::c("Mean above average" = "grey30", "Mean below average" = "grey50")) +
          ggplot2::labs(x = factor_axis, y = "DIFFERENCE", title = title_9) +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white", 
                              label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
          ggplot2::coord_flip(ylim = 1.25 * base::c(-base::max(base::abs(data_chart$mean_group)), base::max(base::abs(data_chart$mean_group)))) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                         legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                         legend.position = "none",
                         legend.box.spacing = ggplot2::unit(0.25, "cm"),
                         legend.text = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         legend.title = ggplot2::element_text(size = text_size, color = "black", face = "bold")) +
          ggplot2::guides(ncol = 1) -> plot9
        
        # ------------------------------------------------------------------------------
        # PLOT 10:
        data %>%
          dplyr::mutate(median_value = stats::median(!!numeric_var, na.rm = TRUE)) %>%
          dplyr::group_by(!!factor_var) %>%
          dplyr::mutate(median_group = stats::median(!!numeric_var, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%          
          dplyr::select(!!factor_var, median_value, median_group) %>%
          dplyr::distinct() %>%
          dplyr::mutate(type = base::ifelse(median_group < median_value, "Median below average", "Median above average")) %>%
          dplyr::arrange(median_group) %>%
          dplyr::mutate(median_group = median_group - median_value) %>%
          dplyr::ungroup() -> data_chart
        ggplot(data = data_chart, aes(x = !!factor_var, y = median_group, label = base::round(median_group, 2))) +
          ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 1 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 1 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 2 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 2 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 3 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_hline(yintercept = 3 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
          ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
          ggplot2::scale_fill_manual(name = "GROUP:", 
                                     labels = base::c("MEDIAN ABOVE AVERAGE", "MEDIAN BELOW AVERAGE"),
                                     values = base::c("Median above average" = "grey30", "Median below average" = "grey50")) +
          ggplot2::labs(x = factor_axis, y = "DIFFERENCE", title = title_10) +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
          ggplot2::coord_flip(ylim = 1.25 * base::c(-base::max(base::abs(data_chart$median_group)), base::max(base::abs(data_chart$median_group)))) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                         legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                         legend.position = "none",
                         legend.box.spacing = ggplot2::unit(0.25, "cm"),
                         legend.text = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         legend.title = ggplot2::element_text(size = text_size, color = "black", face = "bold")) +
          ggplot2::guides(ncol = 1) -> plot10
        
        # ------------------------------------------------------------------------------
        # PLOT 11:
        data %>% dplyr::mutate(cuts = ggplot2::cut_interval(!!numeric_var, n = numeric_cuts, dig.lab = digits_lab)) -> data
        ggplot2::ggplot(data = data, aes(x = cuts, fill = !!factor_var)) +
          ggplot2::geom_bar(position = "fill", color = "black") +  
          ggplot2::labs(x = numeric_axis, y = percentage_axis, title = title_11) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                         legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                         legend.position = "right",
                         legend.box.spacing = ggplot2::unit(0.25, "cm"),
                         legend.text = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         legend.title = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         legend.key = ggplot2::element_rect(color = "gray90")) +
          ggplot2::scale_y_continuous(labels = scales::percent,
                                      limits = base::c(0, 1),
                                      breaks = base::c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) +
          ggplot2::guides(fill = guide_legend(base::paste(factor_axis, ":", sep = ""), ncol = 1)) +
          ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) +
          ggplot2::coord_flip() -> plot11
        
        # ------------------------------------------------------------------------------
        # PLOT 12:
        ggplot2::ggplot(data = data, aes(x = !!numeric_var, color = !!factor_var)) +
          ggplot2::stat_ecdf(lwd = 0.5) +
          ggplot2::labs(x = numeric_axis, y = percentage_axis, title = title_12) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.box.background = ggplot2::element_rect(color = "black", size = 0.5, linetype = "solid"),
                         legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                         legend.position = "right",
                         legend.box.spacing = ggplot2::unit(0.25, "cm"),
                         legend.text = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         legend.title = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         legend.key = ggplot2::element_rect(color = "black", fill = "gray90")) +
          ggplot2::scale_y_continuous(labels = scales::percent,
                                      limits = base::c(0, 1),
                                      breaks = base::c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) +
          ggplot2::guides(color = guide_legend(base::paste(factor_axis, ":", sep = ""), ncol = 1)) +
          ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot12
        
        # ------------------------------------------------------------------------------      
        # PLOT 13:  
        ggplot2::ggplot(data = data) +
          ggplot2::geom_boxplot(aes(x = 1, y = !!numeric_var), fill = "gray60", color = "black", outlier.color = "black", notch = FALSE) + 
          ggplot2::geom_violin(aes(x = 3, y = !!numeric_var), draw_quantiles = base::c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", fill = "gray60", trim = FALSE) +
          ggplot2::stat_summary(aes(x = 1, y = !!numeric_var), fun = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
          ggplot2::stat_summary(aes(x = 1, y = !!numeric_var), fun = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
          ggplot2::stat_summary(aes(x = 3, y = !!numeric_var), fun = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
          ggplot2::stat_summary(aes(x = 3, y = !!numeric_var), fun = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
          ggplot2::labs(x = factor_axis, y = numeric_axis, title = title_13) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_blank(), 
                         axis.ticks.y = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.x = ggplot2::element_blank(),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") +
          ggplot2::scale_x_continuous(limits = base::c(0, 4)) -> plot13
        
        # ------------------------------------------------------------------------------       
        # PLOT 14:
        ggplot2::ggplot(data = data, aes(x = !!numeric_var)) +
          ggplot2::geom_histogram(color = "black", bins = histogram_bins, fill = "gray60") +  
          ggplot2::labs(x = numeric_axis, y = count_axis, title = title_14) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") -> plot14
        
        # ------------------------------------------------------------------------------        
        # PLOT 15:
        ggplot2::ggplot(data = data, aes(sample = !!numeric_var)) +
          ggplot2::stat_qq(size = 0.5) +
          ggplot2::stat_qq_line(lwd = 0.5, color = "black") +
          ggplot2::labs(x = "THEORETICAL",  y = "SAMPLE", title = title_15) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") -> plot15
        
        # ------------------------------------------------------------------------------       
        # PLOT 16:
        ggplot2::ggplot(data = data, aes(x = !!numeric_var)) +
          ggplot2::stat_density(fill = "gray60", color = "black", lwd = 0.5) +
          ggplot2::stat_ecdf(lwd = 1, color = "black") +
          ggplot2::labs(x = numeric_axis, y = percentage_axis, title = title_16, caption = caption) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         panel.grid.major.x = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.major.y = ggplot2::element_line(color = "black", linetype = "dotted"),
                         panel.grid.minor.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.minor.y = ggplot2::element_line(linetype = "blank"),
                         plot.background = ggplot2::element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                         panel.background = ggplot2::element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                         panel.border = ggplot2::element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                         plot.caption = ggplot2::element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                         legend.position = "none") +
          ggplot2::scale_y_continuous(labels = scales::percent, 
                                      limits = base::c(0, 1),
                                      breaks = base::c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) -> plot16
        
        # ------------------------------------------------------------------------------         
        # Save plots:
        plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(plot1, plot2, plot3, plot4,
                                                                plot5, plot6, plot7, plot8,
                                                                plot9, plot10, plot11, plot12,
                                                                plot13, plot14, plot15, plot16, layout_matrix = plot_grid))
        if (save_plots == TRUE){
          if (base::is.null(save_filename)){
            ggplot2::ggsave(filename = base::paste0(base::paste(datetime, factor_var_name, numeric_var_name), save_file_format), 
                            plot = plots, width = save_width, height = save_height, units = save_plots_units, dpi = save_dpi)
            plot_name <- base::paste0(base::paste(datetime, factor_var_name, numeric_var_name), save_file_format)
            base::cat(base::paste(base::getwd(), plot_name, sep = "/")); base::cat("\n")
          } else {
            ggplot2::ggsave(filename = paste0(save_filename, save_file_format), 
                            plot = plots, width = save_width, height = save_height, units = save_plots_units, dpi = save_dpi)
            plot_name <- base::paste0(save_filename, save_file_format)
            base::cat(base::paste(base::getwd(), plot_name, sep = "/")); base::cat("\n")}
        }
      } else {base::cat("ERROR: Type of provided numeric_var is not appropariate (require numeric or integer)"); base::cat("\n")}
    } else {base::cat("ERROR: Type of provided factor_var is not appropariate (require factor)"); base::cat("\n")}
  } else {base::cat("ERROR: Type of provided data is not appropariate (require tibble or dataframe)"); base::cat("\n")}
}

# ------------------------------------------------------------------------------
# https://github.com/ForesightAdamNowacki