# ------------------------------------------------------------------------------
# NUMERIC VS NUMERIC EXPLORATORY DATA ANALYSIS
# ------------------------------------------------------------------------------
plot_numeric_numeric <- function(data, #  data frame or tibble (obligatory parameter)
                                 numeric_var_1, # 1st numeric variable (obligatory parameter)
                                 numeric_var_2, # 2nd numeric variable (obligatory parameter)
                                 variables_as_string = FALSE, # if FALSE factor_var_1 and factor_var_2 without "" 
                                 # if TRUE factor_var_1 and factor_var_2 in ""
                                 
                                 # Sample data from original datatset to make faster function compilation:
                                 data_size = 1.0, # dataset size - e.g 1.0 (= 100% -> all observations), e.g 0.5 (= 50% -> half of observations)
                                 seed_value = 42, # seed for random data generation 
                                 
                                 # Axis, labels:
                                 numeric_axis_1 = NULL, # name of 1st numeric axis
                                 numeric_axis_2 = NULL, # name of 2nd numeric axis
                                 count_axis = "COUNT", # name of count axis
                                 percentage_axis = "PERCENTAGE", # name of percentage axis
                                 density_axis = "DENSITY", # name of density axis
                                 caption = NULL, # caption
                                 
                                 # Titles:
                                 title_1 = "BOX PLOT & VIOLIN PLOT", 
                                 title_2 = "HISTOGRAM",
                                 title_3 = "QUANTILE-QUANTILE PLOT",
                                 title_4 = "DISTRIBUTION PLOT",
                                 title_5 = "SCATTER PLOT",
                                 title_6 = "SCALED SCATTER PLOT PLOT",
                                 title_7 = "HEXAGONAL HEATMAP",
                                 title_8 = "CORRELATION PLOT",
                                 title_9 = "EQUAL RANGE TILE PLOT (N)",
                                 title_10 = "EQUAL RANGE TILE PLOT (%)",
                                 title_11 = "QUANTILE RANGE TILE PLOT (N)",                                 
                                 title_12 = "QUANTILE RANGE TILE PLOT (%)",
                                 title_13 = "BOX PLOT & VIOLIN PLOT",
                                 title_14 = "HISTOGRAM",
                                 title_15 = "QUANTILE-QUANTILE PLOT",
                                 title_16 = "DISTRIBUTION PLOT",
                                 
                                 # Additional parameters:
                                 title_size = 9, # font size for title
                                 text_size = 7, # font size for axises, labels, caption etc.
                                 alpha = 0.25, # transparency in scatter plots
                                 histogram_bars_1 = 5, # number of bars in histogram for 1st numeric variable
                                 histogram_bars_2 = 5, # number of bars in histogram for 2nd numeric variable
                                 hexagonal_bins = 10, # number of horizontal and vertical bins in hexagonal heatmap
                                 cuts_1 = 5, # number of cuts for 1st numeric variable
                                 cuts_2 = 5, # number of cuts for 2nd numeric variable
                                 digits_lab_1 = 2, # decimal numbers in distribution quantile cut plot (1st numeric variable)
                                 digits_lab_2 = 2, # decimal numbers in distribution quantile cut plot (2nd numeric variable)
                                 label_size = 3, # font size for labels
                                 label_percent_round = 1, # number of decimal in percent labels
                                 percentage_breaks = 11, # number of breaks on percentage axis
                                 
                                 # Plot save parameters:
                                 save_plots = FALSE, # FALSE - plot is not saved; TRUE - plot is saved
                                 save_filename = NULL, # filename for saved file; if NULL savefilename consists of variables namesand current datetime
                                 save_width = 64, # default aspekt ratio = 4 * 16:9
                                 save_height = 36, # default aspekt ratio = 4 * 16:9
                                 save_dpi = 100, # plot resolution
                                 save_file_format = ".png", # file save format
                                 save_plots_units = "cm", # file save units
                                 
                                 # Plot matrix:
                                 plot_grid = base::matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), byrow = TRUE, ncol = 4)){ # plot_grid default parameterization is recommended
  
  # ------------------------------------------------------------------------------ 
  # Packages:
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
    numeric_var_1 <- dplyr::enquo(numeric_var_1)                    
    numeric_var_2  <- dplyr::enquo(numeric_var_2) 
    numeric_var_1_name <- dplyr::quo_name(numeric_var_1)
    numeric_var_2_name <- dplyr::quo_name(numeric_var_2)}
  
  if (variables_as_string == TRUE){
    numeric_var_1 <- rlang::sym(numeric_var_1)
    numeric_var_2 <- rlang::sym(numeric_var_2)
    numeric_var_1 <- dplyr::enquo(numeric_var_1)                    
    numeric_var_2  <- dplyr::enquo(numeric_var_2) 
    numeric_var_1_name <- dplyr::quo_name(numeric_var_1)
    numeric_var_2_name <- dplyr::quo_name(numeric_var_2)}
  
  # ------------------------------------------------------------------------------
  # Conditions:
  if (base::is.data.frame(data) | tibble::is_tibble(data)){
    base::cat("INFO: Type of provided data is appropriate"); base::cat("\n")
    if (base::is.numeric(data[[base::which(base::names(data) == numeric_var_1_name)]]) | base::is.integer(data[[base::which(base::names(data) == numeric_var_1_name)]])){
      base::cat("INFO: Type of provided numeric_var_1 is appropariate"); base::cat("\n")
      if (base::is.numeric(data[[base::which(base::names(data) == numeric_var_2_name)]]) | base::is.integer(data[[base::which(base::names(data) == numeric_var_2_name)]])){
        base::cat("INFO: Type of provided numeric_var_2 is appropariate"); base::cat("\n")
        if (base::is.null(numeric_axis_1)){numeric_axis_1 <- "NUMERIC VARIABLE 2"} else {numeric_axis_1 <- stringr::str_to_upper(numeric_axis_1)}
        if (base::is.null(numeric_axis_2)){numeric_axis_2 <- "NUMERIC VARIABLE 2"} else {numeric_axis_2 <- stringr::str_to_upper(numeric_axis_2)}
        if (base::is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(base::paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- dplyr::as_tibble(data)
        base::set.seed(seed = seed_value)
        data %>% dplyr::sample_frac(size = data_size) -> data
        
        # ------------------------------------------------------------------------------
        # PLOT 1:  
        ggplot2::ggplot(data = data) +
          ggplot2::geom_boxplot(aes(x = 1, y = !!numeric_var_1), fill = "gray60", color = "black", outlier.color = "black", notch = FALSE) + 
          ggplot2::geom_violin(aes(x = 3, y = !!numeric_var_1), draw_quantiles = base::c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", fill = "gray60", trim = FALSE) +
          ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_1), fun = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
          ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_1), fun = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
          ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_1), fun = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
          ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_1), fun = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
          ggplot2::labs(y = numeric_axis_1, title = title_1) +
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
          ggplot2::scale_x_continuous(limits = base::c(0, 4)) -> plot1
        
        # ------------------------------------------------------------------------------
        # PLOT 2:
        data %>%
          dplyr::select(!!numeric_var_1) %>%
          dplyr::mutate(cuts = ggplot2::cut_interval(!!numeric_var_1, n = histogram_bars_1, dig.lab = digits_lab_1)) %>%
          dplyr::group_by(cuts) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::ungroup() %>%
          tidyr::complete(cuts, fill = base::list(n = 0)) %>%
          ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = cuts, y = n, label = n)) +
          ggplot2::geom_histogram(stat = "identity", binwidth = 0, width = 1, col = "black", lwd = 0.5, fill = "gray60") +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = ggplot2::unit(0.15, "lines"), label.r = ggplot2::unit(0, "lines")) +
          ggplot2::labs(x = numeric_axis_1, y = count_axis, title = title_1) +
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
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1))-> plot2
        
        # ------------------------------------------------------------------------------
        # PLOT 3:
        ggplot2::ggplot(data = data, aes(sample = !!numeric_var_1)) +
          ggplot2::stat_qq(size = 0.5) +
          ggplot2::stat_qq_line(lwd = 0.5, color = "black") +
          ggplot2::labs(x = "THEORETICAL", y = "SAMPLE", title = title_3) +
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
                         legend.position = "none") -> plot3
        
        # ------------------------------------------------------------------------------
        # PLOT 4:
        ggplot2::ggplot(data = data, aes(x = !!numeric_var_1)) +
          ggplot2::geom_density(aes(!!numeric_var_1, ..scaled..), fill = "gray60", color = "black", lwd = 0.5) +
          ggplot2::stat_ecdf(lwd = 1, color = "black") +
          ggplot2::labs(x = numeric_axis_1, y = percentage_axis, title = title_4) +
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
          ggplot2::scale_y_continuous(limits = base::c(0, 1),
                                      labels = scales::percent_format(accuracy = 1), 
                                      breaks = base::seq(from = 0, to = 1, length.out = percentage_breaks)) -> plot4
        
        # ------------------------------------------------------------------------------         
        # PLOT 5:
        ggplot2::ggplot(data = data, mapping = aes(x = !!numeric_var_1, y = !!numeric_var_2)) +
          ggplot2::geom_point(color = "black", alpha = alpha, size = 1) +
          ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_5) +
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
                         legend.position = "none") -> plot5
        
        # ------------------------------------------------------------------------------           
        # PLOT 6:
        data %>% 
          dplyr::select(numeric_var_1_name, numeric_var_2_name) %>% 
          dplyr::mutate_if(base::is.numeric, function(x) (x - base::min(x))/(base::max(x) - base::min(x))) -> scaled_data
        ggplot(data = scaled_data, aes(x = !!numeric_var_1, y = !!numeric_var_2)) +
          ggplot2::geom_point(color = "black", alpha = alpha, size = 1) +
          ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_6) +
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
          ggplot2::scale_y_continuous(limits = base::c(0, 1),
                                      labels = scales::percent_format(accuracy = 1), 
                                      breaks = base::seq(from = 0, to = 1, length.out = percentage_breaks)) +
          ggplot2::scale_x_continuous(limits = base::c(0, 1),
                                      labels = scales::percent_format(accuracy = 1), 
                                      breaks = base::seq(from = 0, to = 1, length.out = percentage_breaks)) -> plot6
        
        # ------------------------------------------------------------------------------
        # PLOT 7:
        ggplot2::ggplot(data = data, mapping = aes(x = !!numeric_var_1, y = !!numeric_var_2)) +
          ggplot2::geom_hex(bins = hexagonal_bins, colour = "black") +
          ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_7) +
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
                         legend.key = ggplot2::element_rect(color = "gray90", fill = "gray90", size = 0.5)) +
          ggplot2::scale_fill_gradientn(colours = base::c("white", "grey0")) +
          ggplot2::guides(fill = guide_legend("COUNT:")) -> plot7
        
        # ------------------------------------------------------------------------------
        # PLOT 8:
        var_1 <- data[,base::which(names(data) == numeric_var_1_name)]
        var_2 <- data[,base::which(names(data) == numeric_var_2_name)]
        t1 <- tibble::tibble(korelacja = stats::cor(var_1, var_2), V1 = base::factor(1), V2 = base::factor(1))
        colnames(t1)[2:3] <- base::c(numeric_var_1_name, numeric_var_2_name) 
        ggplot2::ggplot(data = t1, mapping = aes(x = numeric_var_1_name, y = numeric_var_2_name, label = round(korelacja, 2))) +
          ggplot2::geom_tile() +
          ggplot2::labs(title = title_8) +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = ggplot2::unit(0.15, "lines"), label.r = ggplot2::unit(0, "lines")) +
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
                         legend.position = "none") +
          ggplot2::scale_fill_grey(start = -1, end = 1) -> plot8
        
        # ------------------------------------------------------------------------------         
        # PLOT 9:
        data %>%
          dplyr::mutate(cuts_1 = ggplot2::cut_interval(!!numeric_var_1, n = cuts_1, dig.lab = digits_lab_1),
                        cuts_2 = ggplot2::cut_interval(!!numeric_var_2, n = cuts_2, dig.lab = digits_lab_2)) %>%
          dplyr::group_by(cuts_1, cuts_2) %>%
          dplyr::summarise(count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          tidyr::complete(cuts_1, cuts_2, fill = base::list(count = 0)) %>%
          ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = count, label = count)) +
          ggplot2::geom_tile(colour = "black") +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = ggplot2::unit(0.15, "lines"), label.r = ggplot2::unit(0, "lines")) +
          ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_9) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
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
          ggplot2::scale_fill_gradientn(colours = base::c("white", "grey0")) +
          ggplot2::guides(fill = guide_legend("COUNT:")) -> plot9
        
        # ------------------------------------------------------------------------------        
        # PLOT 10:
        data %>%
          dplyr::mutate(cuts_1 = ggplot2::cut_interval(!!numeric_var_1, n = cuts_1, dig.lab = digits_lab_1),
                        cuts_2 = ggplot2::cut_interval(!!numeric_var_2, n = cuts_2, dig.lab = digits_lab_2)) %>%
          dplyr::group_by(cuts_1, cuts_2) %>%
          dplyr::summarise(percent = base::round(100 * dplyr::n()/base::nrow(data), label_percent_round)) %>%
          dplyr::ungroup() %>%
          tidyr::complete(cuts_1, cuts_2, fill = base::list(percent = 0)) %>%
          ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = percent, label = base::paste0(percent, "%"))) +
          ggplot2::geom_tile(colour = "black") +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white", 
                              label.padding = ggplot2::unit(0.15, "lines"), label.r = ggplot2::unit(0, "lines")) +
          ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_9) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
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
          ggplot2::scale_fill_gradientn(colours = base::c("white", "grey0")) +
          ggplot2::guides(fill = guide_legend("PERCENTAGE:")) -> plot10
        
        # ------------------------------------------------------------------------------         
        # PLOT 11:
        data %>%
          dplyr::mutate(cuts_1 = ggplot2::cut_number(!!numeric_var_1, n = cuts_1, dig.lab = digits_lab_1), 
                        cuts_2 = ggplot2::cut_number(!!numeric_var_2, n = cuts_2, dig.lab = digits_lab_2)) %>%
          dplyr::group_by(cuts_1, cuts_2) %>%
          dplyr::summarise(count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          tidyr::complete(cuts_1, cuts_2, fill = base::list(count = 0)) %>%
          ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = count, label = count)) +
          ggplot2::geom_tile(colour = "black") +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = ggplot2::unit(0.15, "lines"), label.r = ggplot2::unit(0, "lines")) +
          ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_11) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
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
          ggplot2::scale_fill_gradientn(colours = base::c("white", "grey0")) +
          ggplot2::guides(fill = guide_legend("COUNT:")) -> plot11
        
        # ------------------------------------------------------------------------------ 
        # PLOT 12:
        data %>%
          dplyr::mutate(cuts_1 = ggplot2::cut_number(!!numeric_var_1, n = cuts_1, dig.lab = digits_lab_1),
                        cuts_2 = ggplot2::cut_number(!!numeric_var_2, n = cuts_2, dig.lab = digits_lab_2)) %>%
          dplyr::group_by(cuts_1, cuts_2) %>%
          dplyr::summarise(percent = base::round(100 * dplyr::n()/base::nrow(data), label_percent_round)) %>%
          dplyr::ungroup() %>%
          tidyr::complete(cuts_1, cuts_2, fill = base::list(percent = 0)) %>%
          ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = percent, label = base::paste0(percent, "%"))) +
          ggplot2::geom_tile(colour = "black") +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = ggplot2::unit(0.15, "lines"), label.r = ggplot2::unit(0, "lines")) +
          ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_12) +
          ggplot2::theme(plot.title = ggplot2::element_text(size = title_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                         axis.text.y = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.text.x = ggplot2::element_text(size = text_size, color = "black", face = "plain"),
                         axis.title.y = ggplot2::element_text(size = text_size, color = "black", face = "bold"),
                         axis.title.x = ggplot2::element_text(size = text_size, color = "black", face = "bold"),  
                         axis.ticks = ggplot2::element_line(size = 1, color = "black", linetype = "solid"),
                         axis.ticks.length = ggplot2::unit(0.1, "cm"),
                         panel.grid.major.x = ggplot2::element_line(linetype = "blank"),
                         panel.grid.major.y = ggplot2::element_line(linetype = "blank"),
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
          ggplot2::scale_fill_gradientn(colours = base::c("white", "grey0")) +
          ggplot2::guides(fill = guide_legend("PERCENTAGE:")) -> plot12
        
        # ------------------------------------------------------------------------------
        # PLOT 13:  
        ggplot2::ggplot(data = data) +
          ggplot2::geom_boxplot(aes(x = 1, y = !!numeric_var_2), fill = "gray60", color = "black", outlier.color = "black", notch = FALSE) + 
          ggplot2::geom_violin(aes(x = 3, y = !!numeric_var_2), draw_quantiles = base::c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", fill = "gray60", trim = FALSE) +
          ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_2), fun = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
          ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_2), fun = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
          ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_2), fun = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
          ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_2), fun = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
          ggplot2::labs(y = numeric_axis_2, title = title_1) +
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
        data %>%
          dplyr::select(!!numeric_var_2) %>%
          dplyr::mutate(cuts = ggplot2::cut_interval(!!numeric_var_2, n = histogram_bars_2, dig.lab = digits_lab_2)) %>%
          dplyr::group_by(cuts) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::ungroup() %>%
          tidyr::complete(cuts, fill = base::list(n = 0)) %>%
          ggplot2::ggplot(data = ., mapping = ggplot2::aes(x = cuts, y = n, label = n)) +
          ggplot2::geom_histogram(stat = "identity", binwidth = 0, width = 1, col = "black", lwd = 0.5, fill = "gray60") +
          ggplot2::geom_label(color = "black", size = label_size, label.size = 0.5, fontface = 1, fill = "white",
                              label.padding = ggplot2::unit(0.15, "lines"), label.r = ggplot2::unit(0, "lines")) +
          ggplot2::labs(x = numeric_axis_2, y = count_axis, title = title_14) +
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
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = 0.1)) -> plot14
        
        # ------------------------------------------------------------------------------
        # PLOT 15:
        ggplot2::ggplot(data = data, aes(sample = !!numeric_var_2)) +
          ggplot2::stat_qq(size = 0.5) +
          ggplot2::stat_qq_line(lwd = 0.5, color = "black") +
          ggplot2::labs(x = "THEORETICAL", y = "SAMPLE", title = title_3) +
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
        ggplot2::ggplot(data = data, aes(x = !!numeric_var_2)) +
          ggplot2::geom_density(aes(!!numeric_var_2, ..scaled..), fill = "gray60", color = "black", lwd = 0.5) +
          ggplot2::stat_ecdf(lwd = 0.5, color = "black") +
          ggplot2::labs(x = numeric_axis_2, y = percentage_axis, title = title_4) +
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
          ggplot2::scale_y_continuous(limits = base::c(0, 1),
                                      labels = scales::percent_format(accuracy = 1), 
                                      breaks = base::seq(from = 0, to = 1, length.out = percentage_breaks)) -> plot16
        
        # ------------------------------------------------------------------------------       
        # Save plots:
        plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(plot1, plot2, plot3, plot4,
                                                                plot5, plot6, plot7, plot8,
                                                                plot9, plot10, plot11, plot12,
                                                                plot13, plot14, plot15, plot16, layout_matrix = plot_grid))
        if (save_plots == TRUE){
          if (base::is.null(save_filename)){
            ggplot2::ggsave(filename = base::paste0(base::paste(datetime, numeric_var_1_name, numeric_var_2_name), save_file_format),
                            plot = plots, width = save_width, height = save_height, units = save_plots_units, dpi = save_dpi)
            plot_name <- base::paste0(base::paste(datetime, numeric_var_1_name, numeric_var_2_name), save_file_format)
            base::cat(base::paste(base::getwd(), plot_name, sep = "/")); base::cat("\n")
          } else {
            ggplot2::ggsave(filename =  base::paste0(save_filename, save_file_format),
                            plot = plots, width = save_width, height = save_height, units = save_plots_units, dpi = save_dpi)
            plot_name <- base::paste0(save_filename, save_file_format)
            base::cat(base::paste(base::getwd(), plot_name, sep = "/")); base::cat("\n")}
        }
      } else {base::cat("ERROR: Type of provided numeric_var_2 is not appropariate (require numeric or integer)"); base::cat("\n")}
    } else {base::cat("ERROR: Type of provided numeric_var_1 is not appropariate (require numeric or integer)"); base::cat("\n")}
  } else {base::cat("ERROR: Type of provided data is not appropariate (require tibble or dataframe)"); base::cat("\n")}
}

