# Deep visual analysis numeric vs numeric:
plots_numeric_numeric <- function(data, numeric_var_1, numeric_var_2,  
                                  
                                  # Sample data from original datatset to make faster function compilation:
                                  data_size = 1.0,
                                  seed_value = 42,
                                  
                                  # Axis, labels:
                                  numeric_axis_1 = NULL, numeric_axis_2 = NULL, count_axis = "COUNT", percentage_axis = "PERCENTAGE", density_axis = "DENSITY", caption = NULL,
                                  
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
                                  text_size = 7, title_size = 9, alpha = 0.25, histogram_bins_1 = 10, histogram_bins_2 = 10, hex_bins = 10, cuts_1 = 5, cuts_2 = 5, digits_lab = 8,
                                  
                                  # Plot save parameters:
                                  plots_save = FALSE, save_filename = NULL, save_width = 40, save_height = 40, save_dpi = 500,
                                  
                                  # Plot matrix:
                                  plot_grid = base::matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), byrow = TRUE, ncol = 4)
){
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
  if (!require(openxlsx)){utils::install.packages('openxlsx'); require('openxlsx')}    
  
  # Additional options:
  base::options(scipen = 20)
  
  # Variables:
  numeric_var_1 <- dplyr::enquo(numeric_var_1)                    
  numeric_var_2  <- dplyr::enquo(numeric_var_2) 
  numeric_var_1_name <- dplyr::quo_name(numeric_var_1)
  numeric_var_2_name <- dplyr::quo_name(numeric_var_2)
  
  # Conditions:
  if (base::is.data.frame(data) | tibble::is_tibble(data)){
    print("INFO: Type of provided data is appropriate")
    if (base::is.numeric(data[[base::which(base::names(data) == numeric_var_1_name)]]) | base::is.integer(data[[base::which(base::names(data) == numeric_var_1_name)]])){
      print("INFO: Type of provided numeric_var_1 is appropariate")
      if (base::is.numeric(data[[base::which(base::names(data) == numeric_var_2_name)]]) | base::is.integer(data[[base::which(base::names(data) == numeric_var_2_name)]])){
        print("INFO: Type of provided numeric_var_2 is appropariate")
        if (base::is.null(numeric_axis_1)){numeric_axis_1 <- "NUMERIC VARIABLE 2"} else {numeric_axis_1 <- stringr::str_to_upper(numeric_axis_1)}
        if (base::is.null(numeric_axis_2)){numeric_axis_2 <- "NUMERIC VARIABLE 2"} else {numeric_axis_2 <- stringr::str_to_upper(numeric_axis_2)}
        if (base::is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(base::paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- dplyr::as_tibble(data)
        base::set.seed(seed = seed_value)
        data %>% dplyr::sample_frac(size = data_size) -> data
################################################################################
          # PLOT 1:  
          ggplot2::ggplot(data = data) +
            ggplot2::geom_boxplot(aes(x = 1, y = !!numeric_var_1), fill = "gray60", color = "black", outlier.color = "black", notch = FALSE) + 
            ggplot2::geom_violin(aes(x = 3, y = !!numeric_var_1), draw_quantiles = c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", fill = "gray60", trim = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_1), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_1), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_1), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_1), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::labs(y = numeric_axis_1, title = title_1) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_blank(),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_blank(), 
                           axis.ticks.y = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.x = element_blank(),
                           axis.ticks.length = unit(0.1, "cm"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") +
            ggplot2::scale_x_continuous(limits = c(0, 4)) -> plot1
################################################################################ 
          # PLOT 2:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var_1)) +
            ggplot2::geom_histogram(color = "black", bins = histogram_bins_1, fill = "gray60") +  
            ggplot2::labs(x = numeric_axis_1, y = count_axis, title = title_2) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") -> plot2
################################################################################
          # PLOT 3:
          ggplot2::ggplot(data = data, aes(sample = !!numeric_var_1)) +
            ggplot2::stat_qq(size = 0.5) +
            ggplot2::stat_qq_line(lwd = 0.5, color = "black") +
            ggplot2::labs(x = "THEORETICAL", y = "SAMPLE", title = title_3) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") -> plot3
################################################################################
          # PLOT 4:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var_1)) +
            ggplot2::stat_density(fill = "gray60", color = "black", lwd = 0.5) +
            ggplot2::stat_ecdf(lwd = 1, color = "black") +
            ggplot2::labs(x = numeric_axis_1, y = percentage_axis, title = title_4) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") +
            ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) -> plot4
################################################################################          
          # PLOT 5:
          ggplot2::ggplot(data = data, mapping = aes(x = !!numeric_var_1, y = !!numeric_var_2)) +
            ggplot2::geom_point(color = "black", alpha = alpha, size = 1) +
            ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_5) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") -> plot5
################################################################################           
          # PLOT 6:
          data %>% 
            dplyr::select(numeric_var_1_name, numeric_var_2_name) %>% 
            dplyr::mutate_if(base::is.numeric, function(x) (x - base::min(x))/(base::max(x) - base::min(x))) -> scaled_data
          ggplot(data = scaled_data, aes(x = !!numeric_var_1, y = !!numeric_var_2)) +
            ggplot2::geom_point(color = "black", alpha = alpha, size = 1) +
            ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_6) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") +
            ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) +
            ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, 1), breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) -> plot6
################################################################################
          # PLOT 7:
          ggplot2::ggplot(data = data, mapping = aes(x = !!numeric_var_1, y = !!numeric_var_2)) +
            ggplot2::geom_hex(bins = hex_bins, colour = "black") +
            ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_7) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.box.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
                           legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                           legend.position = "right",
                           legend.box.spacing = unit(0.25, "cm"),
                           legend.text = element_text(size = text_size, color = "black", face = "plain"),
                           legend.title = element_text(size = text_size, color = "black", face = "bold"),
                           legend.key = element_rect(color = "gray90", fill = "gray90", size = 0.5)) +
            ggplot2::scale_fill_gradientn(colours = c("white", "grey0")) +
            ggplot2::guides(fill = guide_legend("COUNT:")) -> plot7
################################################################################
          # PLOT 8:
          var_1 <- data[,base::which(names(data) == numeric_var_1_name)]
          var_2 <- data[,base::which(names(data) == numeric_var_2_name)]
          t1 <- tibble::tibble(korelacja = stats::cor(var_1, var_2), V1 = base::factor(1), V2 = base::factor(1))
          colnames(t1)[2:3] <- c(numeric_var_1_name, numeric_var_2_name) 
          ggplot2::ggplot(data = t1, mapping = aes(x = numeric_var_1_name, y = numeric_var_2_name, label = round(korelacja, 2))) +
            ggplot2::geom_tile() +
            ggplot2::labs(title = title_8) +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.title.x = element_blank(),
                           axis.text.x = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           panel.grid.major.x = element_line(linetype = "blank"),
                           panel.grid.major.y = element_line(linetype = "blank"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") +
            ggplot2::scale_fill_grey(start = -1, end = 1) -> plot8
################################################################################          
          # PLOT 9:
          data %>%
            dplyr::mutate(cuts_1 = ggplot2::cut_interval(!!numeric_var_1, n = cuts_1, dig.lab = digits_lab), cuts_2 = ggplot2::cut_interval(!!numeric_var_2, n = cuts_2, dig.lab = digits_lab)) %>%
            dplyr::group_by(cuts_1, cuts_2) %>%
            dplyr::summarise(count = dplyr::n()) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts_1, cuts_2, fill = base::list(count = 0)) %>%
            ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = count, label = count)) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_9) +

            
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           panel.grid.major.x = element_line(linetype = "blank"),
                           panel.grid.major.y = element_line(linetype = "blank"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.box.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
                           legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                           legend.position = "none",
                           legend.box.spacing = unit(0.25, "cm"),
                           legend.text = element_text(size = text_size, color = "black", face = "plain"),
                           legend.title = element_text(size = text_size, color = "black", face = "bold")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "grey0")) +
            ggplot2::guides(fill = guide_legend("COUNT:")) -> plot9
################################################################################          
          # PLOT 10:
          data %>%
            dplyr::mutate(cuts_1 = ggplot2::cut_interval(!!numeric_var_1, n = cuts_1, dig.lab = digits_lab), cuts_2 = ggplot2::cut_interval(!!numeric_var_2, n = cuts_2, dig.lab = digits_lab)) %>%
            dplyr::group_by(cuts_1, cuts_2) %>%
            dplyr::summarise(percent = base::round(100 * dplyr::n()/base::nrow(data), 2)) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts_1, cuts_2, fill = base::list(percent = 0)) %>%
            ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = percent, label = base::paste0(percent, "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_9) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           panel.grid.major.x = element_line(linetype = "blank"),
                           panel.grid.major.y = element_line(linetype = "blank"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.box.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
                           legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                           legend.position = "none",
                           legend.box.spacing = unit(0.25, "cm"),
                           legend.text = element_text(size = text_size, color = "black", face = "plain"),
                           legend.title = element_text(size = text_size, color = "black", face = "bold")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "grey0")) +
            ggplot2::guides(fill = guide_legend("PERCENTAGE:")) -> plot10
################################################################################          
          # PLOT 11:
          data %>%
            dplyr::mutate(cuts_1 = ggplot2::cut_number(!!numeric_var_1, n = cuts_1, dig.lab = digits_lab), cuts_2 = ggplot2::cut_number(!!numeric_var_2, n = cuts_2)) %>%
            dplyr::group_by(cuts_1, cuts_2) %>%
            dplyr::summarise(count = dplyr::n()) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts_1, cuts_2, fill = base::list(count = 0)) %>%
            ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = count, label = count)) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_11) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           panel.grid.major.x = element_line(linetype = "blank"),
                           panel.grid.major.y = element_line(linetype = "blank"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.box.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
                           legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                           legend.position = "none",
                           legend.box.spacing = unit(0.25, "cm"),
                           legend.text = element_text(size = text_size, color = "black", face = "plain"),
                           legend.title = element_text(size = text_size, color = "black", face = "bold")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "grey0")) +
            ggplot2::guides(fill = guide_legend("COUNT:")) -> plot11
################################################################################  
          # PLOT 12:
          data %>%
            dplyr::mutate(cuts_1 = ggplot2::cut_number(!!numeric_var_1, n = cuts_1, dig.lab = digits_lab), cuts_2 = ggplot2::cut_number(!!numeric_var_2, n = cuts_2)) %>%
            dplyr::group_by(cuts_1, cuts_2) %>%
            dplyr::summarise(percent = base::round(100 * dplyr::n()/base::nrow(data), 2)) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts_1, cuts_2, fill = base::list(percent = 0)) %>%
            ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = percent, label = base::paste0(percent, "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::labs(x = numeric_axis_1, y = numeric_axis_2, title = title_12) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           panel.grid.major.x = element_line(linetype = "blank"),
                           panel.grid.major.y = element_line(linetype = "blank"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.box.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
                           legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                           legend.position = "none",
                           legend.box.spacing = unit(0.25, "cm"),
                           legend.text = element_text(size = text_size, color = "black", face = "plain"),
                           legend.title = element_text(size = text_size, color = "black", face = "bold")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "grey0")) +
            ggplot2::guides(fill = guide_legend("PERCENTAGE:")) -> plot12
################################################################################ 
          # PLOT 13:  
          ggplot2::ggplot(data = data) +
            ggplot2::geom_boxplot(aes(x = 1, y = !!numeric_var_2), fill = "gray60", color = "black", outlier.color = "black", notch = FALSE) + 
            ggplot2::geom_violin(aes(x = 3, y = !!numeric_var_2), draw_quantiles = c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", fill = "gray60", trim = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_2), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_2), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_2), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_2), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::labs(y = numeric_axis_2, title = title_1) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_blank(),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_blank(), 
                           axis.ticks.y = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.x = element_blank(),
                           axis.ticks.length = unit(0.1, "cm"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") +
            ggplot2::scale_x_continuous(limits = c(0, 4)) -> plot13
################################################################################ 
          # PLOT 14:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var_2)) +
            ggplot2::geom_histogram(color = "black", bins = histogram_bins_2, fill = "gray60") +  
            ggplot2::labs(x = numeric_axis_2, y = count_axis, title = title_2) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") -> plot14
################################################################################
          # PLOT 15:
          ggplot2::ggplot(data = data, aes(sample = !!numeric_var_2)) +
            ggplot2::stat_qq(size = 0.5) +
            ggplot2::stat_qq_line(lwd = 0.5, color = "black") +
            ggplot2::labs(x = "THEORETICAL", y = "SAMPLE", title = title_3) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") -> plot15
################################################################################
          # PLOT 16:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var_2)) +
            ggplot2::stat_density(fill = "gray60", color = "black", lwd = 0.5) +
            ggplot2::stat_ecdf(lwd = 0.5, color = "black") +
            ggplot2::labs(x = numeric_axis_2, y = percentage_axis, title = title_4) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.1, "cm"),
                           panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                           panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") +
            ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) -> plot16
################################################################################          
          # grDevices::dev.new()
          plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(plot1, plot2, plot3, plot4,
                                                                  plot5, plot6, plot7, plot8,
                                                                  plot9, plot10, plot11, plot12,
                                                                  plot13, plot14, plot15, plot16, layout_matrix = plot_grid))
          if (plots_save == TRUE){
            if (base::is.null(save_filename)){
              ggplot2::ggsave(filename = base::paste0(base::paste(base::Sys.Date(), numeric_var_1_name, numeric_var_2_name), ".png"), plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
              plot_name <- base::paste0(base::paste(base::Sys.Date(), numeric_var_1_name, numeric_var_2_name), ".png")
              base::print(base::paste(base::getwd(), plot_name, sep = "/"))
            } else {
              ggplot2::ggsave(filename =  base::paste0(save_filename, ".png"), plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
              plot_name <- base::paste0(save_filename, ".png")
              base::print(base::paste(base::getwd(), plot_name, sep = "/"))
            }
          }
################################################################################  
      } else {
        base::print("ERROR: Type of provided numeric_var_2 is not appropariate (require numeric or integer)")}
    } else {
      base::print("ERROR: Type of provided numeric_var_1 is not appropariate (require numeric or integer)")}
  } else {
    base::print("ERROR: Type of provided data is not appropariate (require tibble or dataframe)")}
}

# plots_numeric_numeric(data = diamonds,
#                       data_size = 0.1,
#                       cuts_1 = 4, cuts_2 = 5,
#                       numeric_var_1 = price, numeric_var_2 = carat,
#                       numeric_axis_1 = "price", numeric_axis_2 = "carat")