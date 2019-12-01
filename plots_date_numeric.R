# Deep visual analysis date vs numeric:
plots_date_numeric <- function(data, date_var, numeric_var,  
                               
                               # Sample data from original datatset to make faster function compilation:
                               data_size = 1.0,
                               seed_value = 42,
                               
                               # Axis, labels:
                               date_axis = NULL, numeric_axis = NULL, year_axis = "YEAR", month_axis = "MONTH", day_axis = "WEEK DAY", caption = NULL,
                               labels_month = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                               labels_week_day = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                               
                               # Titles:
                               title_1 = "SCATTER PLOT", 
                               title_2 = "TREND PLOT",
                               title_4 = "PEAK & VALLEY PLOT",
                               title_5 = "DIFFERENCE OF GROUP MEAN TO TOTAL MEAN - YEAR PLOT",
                               title_6 = "DIFFERENCE OF GROUP MEDIAN TO TOTAL MEDIAN - YEAR PLOT",
                               title_7 = "MEAN TILE PLOT - YEAR VS MONTH",
                               title_8 = "YEAR TO YEAR MEAN DIFFERENCE PLOT",                               
                               title_9 = "DIFFERENCE OF GROUP MEAN TO TOTAL MEAN - MONTH PLOT",
                               title_10 = "DIFFERENCE OF GROUP MEDIAN TO TOTAL MEDIAN - MONTH PLOT",    
                               title_11 = "MEAN TILE PLOT - YEAR VS WEEK DAY",
                               title_12 = "MONTH TO MONTH MEAN DIFFERENCE PLOT",                              
                               title_13 = "DIFFERENCE OF GROUP MEAN TO TOTAL MEAN - WEEK DAY PLOT",
                               title_14 = "DIFFERENCE OF GROUP MEDIAN TO TOTAL MEDIAN - WEEK DAY PLOT",    
                               title_15 = "MEAN TILE PLOT - MONTH VS WEEK DAY",                               
                               title_16 = "DAY TO DAY MEAN DIFFERENCE PLOT",
                               
                               # Additional parameters:
                               text_size = 7, title_size = 9, alpha = 0.5, span = 151, cuts_1 = 5, cuts_2 = 5, 

                               # Plot save parameters:
                               plots_save = FALSE, save_filename = NULL, save_width = 40, save_height = 40, save_dpi = 500,
                               
                               # Plot matrix:
                               plot_grid = base::matrix(c(1, 2, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), byrow = TRUE, ncol = 4)
                               
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
  if (!require(hexbin)){utils::install.packages('hexbin'); require('hexbin')} 
  if (!require(lubridate)){utils::install.packages('lubridate'); require('lubridate')} 
  if (!require(ggpmisc)){utils::install.packages('ggpmisc'); require('ggpmisc')} 
  if (!require(ggrepel)){utils::install.packages('ggrepel'); require('ggrepel')} 
  
  # Additional options:
  base::options(scipen = 20)
  
  # Variables:
  date_var <- dplyr::enquo(date_var)                    
  numeric_var  <- dplyr::enquo(numeric_var) 
  date_var_name <- quo_name(date_var)
  numeric_var_name <- quo_name(numeric_var)
  
  # Conditions:
  if (base::is.data.frame(data) | tibble::is_tibble(data)){
    base::print("INFO: Type of provided data is appropriate")
    if (lubridate::is.Date(data[[base::which(names(data) == date_var_name)]])){
      base::print("INFO: Type of provided date_var is appropariate")
      if (base::is.numeric(data[[base::which(base::names(data) == numeric_var_name)]]) | base::is.integer(data[[base::which(base::names(data) == numeric_var_name)]])){
        base::print("INFO: Type of provided numeric_var is appropariate")
        if (base::is.null(date_axis)){date_axis <- "DATE VARIABLE"} else {date_axis <- stringr::str_to_upper(date_axis)}
        if (base::is.null(numeric_axis)){numeric_axis <- "NUMERIC VARIABLE"} else {numeric_axis <- stringr::str_to_upper(numeric_axis)}
        if (base::is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- tibble::as_tibble(data)
        base::set.seed(seed = seed_value)
        data %>% dplyr::sample_frac(size = data_size) -> data
################################################################################ 
          # PLOT 1:
          data %>% dplyr::select(!!numeric_var) -> var; var <- c(var); var <- base::unlist(var)
            ggplot2::ggplot(data = data, mapping = aes(x = !!date_var, y = !!numeric_var)) +
              ggplot2::geom_point(alpha = alpha, size = 0.2) +
              ggplot2::geom_hline(yintercept = mean(var, na.rm = TRUE), lty = 1, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = quantile(var, probs = 0.25, na.rm = TRUE), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = quantile(var, probs = 0.50, na.rm = TRUE), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = quantile(var, probs = 0.75, na.rm = TRUE), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::labs(x = date_axis, y = numeric_axis, title = title_1) +
              ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                             axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                             axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                             axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                             axis.title.x = element_text(size = text_size, color = "black", face = "bold"),
                             axis.ticks.y = element_line(size = 1, color = "black", linetype = "solid"),
                             axis.ticks.x = element_line(size = 1, color = "black", linetype = "solid"),
                             axis.ticks.length = unit(0.1, "cm"),
                             plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                             panel.background = element_rect(fill = "gray90", color = "black", size = 0.5, linetype = "solid"),
                             panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                             panel.grid.major.x = element_line(color = "black", linetype = "dotted"),
                             panel.grid.major.y = element_line(color = "black", linetype = "dotted"),
                             panel.grid.minor.x = element_line(linetype = "blank"),
                             panel.grid.minor.y = element_line(linetype = "blank"),
                             plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                             legend.position = "none") -> plot1
################################################################################             
            # PLOT 2:
            ggplot2::ggplot(data = data, mapping = aes(x = !!date_var, y = !!numeric_var)) +
              ggplot2::geom_point(alpha = alpha, size = 0.5) +
              ggplot2::geom_smooth(color = "white", method = "lm", se = FALSE,  lty = 1, lwd = 1.0) +
              ggplot2::geom_smooth(color = "white", method = "loess", se = FALSE, lty = 1, lwd = 1.0) +
              ggplot2::geom_smooth(color = "black", method = "lm", se = FALSE,  lty = 1, lwd = 0.5) +
              ggplot2::geom_smooth(color = "black", method = "loess", se = FALSE, lty = 1, lwd = 0.5) +
              ggplot2::labs(x = date_axis, y = numeric_axis, title = title_2) +
              ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                             axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                             axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                             axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                             axis.title.x = element_text(size = text_size, color = "black", face = "bold"),
                             axis.ticks.y = element_line(size = 1, color = "black", linetype = "solid"),
                             axis.ticks.x = element_line(size = 1, color = "black", linetype = "solid"),
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
            # PLOT 4:
            data %>%
              ggplot2::ggplot(data = ., mapping = aes(x = !!date_var, y = !!numeric_var, label = base::paste(base::round(!!numeric_var, 2), ":", !!date_var))) +
              ggplot2::geom_line(color = "black", lwd = 0.5) + 
              ggpmisc::stat_peaks(colour = "black", span = span) +
              ggpmisc::stat_peaks(geom = "label_repel", span = span, x.label.fmt = "%Y-%m-%d", colour = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggpmisc::stat_valleys(colour = "black", span = span) +
              ggpmisc::stat_valleys(geom = "label_repel", span = span, x.label.fmt = "%Y-%m-%d", colour = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::labs(x = date_axis, y = numeric_axis, title = title_4) +
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
                             legend.title = element_text(size = text_size, color = "black", face = "bold")) -> plot4
################################################################################ 
            # PLOT 5:
            data %>%
              dplyr::mutate(mean_value = base::mean(!!numeric_var, na.rm = TRUE), year = base::factor(lubridate::year(!!date_var))) %>%
              dplyr::group_by(year) %>%
              dplyr::mutate(mean_group = base::mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::select(year, mean_value, mean_group) %>%
              dplyr::distinct() %>%
              dplyr::mutate(type = base::ifelse(mean_group < mean_value, "Mean below average", "Mean above average")) %>%
              dplyr::arrange(mean_group) %>%
              dplyr::mutate(mean_group = mean_group - mean_value) %>%
              dplyr::ungroup() -> data_chart
            ggplot(data = data_chart, aes(x = year, y = mean_group, label = base::round(mean_group, 2))) +
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::scale_fill_manual(values = c("Mean above average" = "grey30", "Mean below average" = "grey50")) +
              ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
              ggplot2::labs(x = year_axis, y = "DIFFERENCE", title = title_5) +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::coord_flip(ylim = 1.25 * c(-max(abs(data_chart$mean_group)), max(abs(data_chart$mean_group)))) +
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
                             legend.position = "none",
                             legend.box.spacing = unit(0.25, "cm"),
                             legend.text = element_text(size = text_size, color = "black", face = "plain"),
                             legend.title = element_text(size = text_size, color = "black", face = "bold"),
                             legend.key = element_rect(size = 4, fill = "gray90", colour = "gray90"), 
                             legend.key.size = unit(0.75, "cm")) +
              ggplot2::guides(ncol = 1)  -> plot5
################################################################################
            # PLOT 6:
            data %>%
              dplyr::mutate(median_value = stats::median(!!numeric_var, na.rm = TRUE), year = base::factor(lubridate::year(!!date_var))) %>%
              dplyr::group_by(year) %>%
              dplyr::mutate(median_group = stats::median(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::select(year, median_value, median_group) %>%
              dplyr::distinct() %>%
              dplyr::mutate(type = base::ifelse(median_group < median_value, "Median below average", "Median above average")) %>%
              dplyr::arrange(median_group) %>%
              dplyr::mutate(median_group = median_group - median_value) %>%
              dplyr::ungroup() -> data_chart
            ggplot(data = data_chart, aes(x = year, y = median_group, label = base::round(median_group, 2))) +
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::scale_fill_manual(values = c("Median above average" = "grey30", "Median below average" = "grey50")) +
              ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
              ggplot2::labs(x = year_axis, y = "DIFFERENCE", title = title_6) +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::coord_flip(ylim = 1.25 * c(-max(abs(data_chart$median_group)), max(abs(data_chart$median_group)))) +
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
                             legend.position = "none",
                             legend.box.spacing = unit(0.25, "cm"),
                             legend.text = element_text(size = text_size, color = "black", face = "plain"),
                             legend.title = element_text(size = text_size, color = "black", face = "bold")) +
              ggplot2::guides(ncol = 1) -> plot6
################################################################################            
            # PLOT 7:
            data %>%
              dplyr::mutate(year = base::factor(lubridate::year(!!date_var)), month = base::factor(lubridate::month(!!date_var), levels = 1:12, labels = labels_month)) %>%
              dplyr::group_by(year, month) %>%
              dplyr::summarise(mean = mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() -> tab
            ggplot2::ggplot(data = tab, mapping = aes(x = year, y = month, fill = mean, label = base::round(mean, 2))) +
              ggplot2::geom_tile(colour = "black") +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::labs(x = year_axis, y = month_axis, title = title_7, fill = "MEAN VALUE:") +
              ggplot2::scale_fill_gradient(low = "white", high = "black", limits = c(base::min(tab$mean), base::max(tab$mean))) + 
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
              ggplot2::guides(ncol = 1) -> plot7
################################################################################   
            # PLOT 8:
            data %>%
              dplyr::select(!!date_var, !!numeric_var) %>%
              dplyr::mutate(year = lubridate::year(!!date_var)) %>%
              dplyr::group_by(year) %>%
              dplyr::summarise(mean = base::mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(date = lubridate::make_date(year, 1, 1)) %>%
              dplyr::mutate(difference = mean - dplyr::lag(mean, n = 1), lag_date = dplyr::lag(date, n = 1)) %>%
              dplyr::filter(!base::is.na(difference)) %>%
              dplyr::mutate(type = base::ifelse(difference < 0, "decrease", "increase")) -> tab
            ggplot2::ggplot(data = tab, mapping = aes(x = date, y = difference,  fill = type)) + 
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(stat = "identity", colour = "black") +
              ggplot2::labs(x = date_axis, y = "DIFFERENCE", title = title_8) +
              ggplot2::scale_fill_manual(values = c("increase" = "grey30", "decrease" = "grey50")) +
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
                             legend.position = "none",
                             legend.box.spacing = unit(0.25, "cm"),
                             legend.text = element_text(size = text_size, color = "black", face = "plain"),
                             legend.title = element_text(size = text_size, color = "black", face = "bold")) +
              ggplot2::coord_cartesian(ylim = c(-base::max(base::abs(tab$difference)), base::max(base::abs(tab$difference)))) -> plot8
################################################################################             
            # PLOT 9:
            data %>%
              dplyr::mutate(mean_value = base::mean(!!numeric_var, na.rm = TRUE), month = base::factor(lubridate::month(!!date_var), levels = 1:12, labels = labels_month)) %>%
              dplyr::group_by(month) %>%
              dplyr::mutate(mean_group = base::mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::select(month, mean_value, mean_group) %>%
              dplyr::distinct() %>%
              dplyr::mutate(type = base::ifelse(mean_group < mean_value, "Mean below average", "Mean above average")) %>%
              dplyr::arrange(mean_group) %>%
              dplyr::mutate(mean_group = mean_group - mean_value) %>%
              dplyr::ungroup() -> data_chart
            ggplot(data = data_chart, aes(x = month, y = mean_group, label = base::round(mean_group, 2))) +
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
              ggplot2::scale_fill_manual(values = c("Mean above average" = "grey30", "Mean below average" = "grey50")) +
              ggplot2::labs(x = month_axis, y = "DIFFERENCE", title = title_9) +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::coord_flip(ylim = 1.25 * c(-max(abs(data_chart$mean_group)), max(abs(data_chart$mean_group)))) +
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
                             legend.position = "none",
                             legend.box.spacing = unit(0.25, "cm"),
                             legend.text = element_text(size = text_size, color = "black", face = "plain"),
                             legend.title = element_text(size = text_size, color = "black", face = "bold")) +
              ggplot2::guides(ncol = 1) -> plot9
################################################################################            
            # PLOT 10:
            data %>%
              dplyr::mutate(median_value = stats::median(!!numeric_var, na.rm = TRUE), month = base::factor(lubridate::month(!!date_var), levels = 1:12, labels = labels_month)) %>%
              dplyr::group_by(month) %>%
              dplyr::mutate(median_group = stats::median(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::select(month, median_value, median_group) %>%
              dplyr::distinct() %>%
              dplyr::mutate(type = base::ifelse(median_group < median_value, "Median below average", "Median above average")) %>%
              dplyr::arrange(median_group) %>%
              dplyr::mutate(median_group = median_group - median_value) %>%
              dplyr::ungroup() -> data_chart
            ggplot(data = data_chart, aes(x = month, y = median_group, label = round(median_group, 2))) +
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
              ggplot2::scale_fill_manual(values = c("Median above average" = "grey30", "Median below average" = "grey50")) +
              ggplot2::labs(x = month_axis, y = "DIFFERENCE", title = title_10) +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::coord_flip(ylim = 1.25 * c(-base::max(base::abs(data_chart$median_group)), base::max(base::abs(data_chart$median_group)))) +
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
                             legend.position = "none",
                             legend.box.spacing = unit(0.25, "cm"),
                             legend.text = element_text(size = text_size, color = "black", face = "plain"),
                             legend.title = element_text(size = text_size, color = "black", face = "bold")) +
              ggplot2::guides(ncol = 1) -> plot10
################################################################################            
            # PLOT 11:
            data %>%
              dplyr::mutate(year = base::factor(lubridate::year(!!date_var)), day = base::factor(lubridate::wday(!!date_var, week_start = 1), levels = 1:7, labels = labels_week_day)) %>%
              dplyr::group_by(year, day) %>%
              dplyr::summarise(mean = base::mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() -> tab
            tab %>%
              ggplot2::ggplot(data = ., mapping = aes(x = day, y = year, fill = mean, label = base::round(mean, 2))) +
              ggplot2::geom_tile(colour = "black") +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::labs(x = day_axis, y = year_axis, title = title_11, fill = "MEAN VALUE:") +
              ggplot2::scale_fill_gradient(low = "white", high = "black", limits = c(base::min(tab$mean), base::max(tab$mean))) + 
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
              ggplot2::guides(ncol = 1) -> plot11
################################################################################               
            # PLOT 12:
            data %>%
              dplyr::select(!!date_var, !!numeric_var) %>%
              dplyr::mutate(year = lubridate::year(!!date_var), month = lubridate::month(!!date_var)) %>%
              dplyr::group_by(year, month) %>%
              dplyr::summarise(mean = base::mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(date = lubridate::make_date(year, month, 1)) %>%
              dplyr::mutate(difference = mean - dplyr::lag(mean, n = 1), lag_date = dplyr::lag(date, n = 1)) %>%
              dplyr::filter(!base::is.na(difference)) %>%
              dplyr::mutate(type = base::ifelse(difference < 0, "decrease", "increase")) -> tab
            ggplot2::ggplot(data = tab, mapping = aes(x = date, y = difference,  fill = type)) + 
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(stat = "identity", colour = "black") +
              ggplot2::labs(x = date_axis, y = "DIFFERENCE", title = title_12) +
              ggplot2::scale_fill_manual(values = c("increase" = "grey30", "decrease" = "grey50")) +
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
                             legend.position = "none",
                             legend.box.spacing = unit(0.25, "cm"),
                             legend.text = element_text(size = text_size, color = "black", face = "plain"),
                             legend.title = element_text(size = text_size, color = "black", face = "bold")) +
              ggplot2::coord_cartesian(ylim = c(-base::max(base::abs(tab$difference)), base::max(base::abs(tab$difference)))) -> plot12
################################################################################
            # PLOT 13:
            data %>%
              dplyr::mutate(mean_value = base::mean(!!numeric_var, na.rm = TRUE), day = base::factor(lubridate::wday(!!date_var, week_start = 1), levels = 1:7, labels = labels_week_day)) %>%
              dplyr::group_by(day) %>%
              dplyr::mutate(mean_group = base::mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::select(day, mean_value, mean_group) %>%
              dplyr::distinct() %>%
              dplyr::mutate(type = base::ifelse(mean_group < mean_value, "Mean below average", "Mean above average")) %>%
              dplyr::arrange(mean_group) %>%
              dplyr::mutate(mean_group = mean_group - mean_value) %>%
              dplyr::ungroup() -> data_chart
            ggplot(data = data_chart, aes(x = day, y = mean_group, label = round(mean_group, 2))) +
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -stats::sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
              ggplot2::scale_fill_manual(values = c("Mean above average" = "grey30", "Mean below average" = "grey50")) +
              ggplot2::labs(x = day_axis, y = "DIFFERENCE", title = title_13) +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::coord_flip(ylim = 1.25 * c(-base::max(base::abs(data_chart$mean_group)), base::max(base::abs(data_chart$mean_group)))) +
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
                             legend.position = "none",
                             legend.box.spacing = unit(0.25, "cm"),
                             legend.text = element_text(size = text_size, color = "black", face = "plain"),
                             legend.title = element_text(size = text_size, color = "black", face = "bold")) +
              ggplot2::guides(ncol = 1) -> plot13
################################################################################
            # PLOT 14:
            data %>%
              dplyr::mutate(median_value = stats::median(!!numeric_var, na.rm = TRUE), day = base::factor(lubridate::wday(!!date_var, week_start = 1), levels = 1:7, labels = labels_week_day)) %>%
              dplyr::group_by(day) %>%
              dplyr::mutate(median_group = median(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::select(day, median_value, median_group) %>%
              dplyr::distinct() %>%
              dplyr::mutate(type = base::ifelse(median_group < median_value, "Median below average", "Median above average")) %>%
              dplyr::arrange(median_group) %>%
              dplyr::mutate(median_group = median_group - median_value) %>%
              dplyr::ungroup() -> data_chart
            ggplot(data = data_chart, aes(x = day, y = median_group, label = base::round(median_group, 2))) +
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -stats::sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
              ggplot2::scale_fill_manual(values = c("Median above average" = "grey30", "Median below average" = "grey50")) +
              ggplot2::labs(x = day_axis, y = "DIFFERENCE", title = title_14) +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::coord_flip(ylim = 1.25 * c(-base::max(base::abs(data_chart$median_group)), base::max(base::abs(data_chart$median_group)))) +
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
                             legend.position = "none",
                             legend.box.spacing = unit(0.25, "cm"),
                             legend.text = element_text(size = text_size, color = "black", face = "plain"),
                             legend.title = element_text(size = text_size, color = "black", face = "bold")) +
              ggplot2::guides(ncol = 1) -> plot14
################################################################################            
            # PLOT 15:
            data %>%
              dplyr::mutate(month = base::factor(lubridate::month(!!date_var), levels = 1:12, labels = labels_month), day = base::factor(lubridate::wday(!!date_var, week_start = 1), levels = 1:7, labels = labels_week_day)) %>%
              dplyr::group_by(month, day) %>%
              dplyr::summarise(mean = base::mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() -> tab
            tab %>%
              ggplot2::ggplot(data = ., mapping = aes(x = day, y = month, fill = mean, label = base::round(mean, 2))) +
              ggplot2::geom_tile(colour = "black") +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::labs(x = day_axis, y = month_axis, title = title_15, fill = "MEAN VALUE:") +
              ggplot2::scale_fill_gradient(low = "white", high = "black", limits = c(min(tab$mean), max(tab$mean))) + 
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
              ggplot2::guides(ncol = 1) -> plot15
################################################################################ 
            # PLOT 16:
            data %>%
              dplyr::select(!!date_var, !!numeric_var) %>%
              dplyr::mutate(difference = !!numeric_var - dplyr::lag(!!numeric_var, n = 1), lag_date = dplyr::lag(!!date_var, n = 1)) %>%
              dplyr::filter(!base::is.na(difference)) %>%
              dplyr::mutate(type = base::ifelse(difference < 0, "decrease", "increase")) -> tab
            ggplot2::ggplot(data = tab, mapping = aes(x = !!date_var, y = difference,  fill = type)) + 
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -stats::sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(stat = "identity") +
              ggplot2::labs(x = date_axis, y = "DIFFERENCE", title = title_16) +
              ggplot2::scale_fill_manual(values = c("increase" = "grey30", "decrease" = "grey50")) +
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
                             legend.position = "none",
                             legend.box.spacing = unit(0.25, "cm"),
                             legend.text = element_text(size = text_size, color = "black", face = "plain"),
                             legend.title = element_text(size = text_size, color = "black", face = "bold")) +
              ggplot2::coord_cartesian(ylim = c(-base::max(base::abs(tab$difference)), base::max(base::abs(tab$difference)))) -> plot16
################################################################################             
            grDevices::dev.new()
            plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(plot1, plot2, plot4,
                                                                   plot5, plot6, plot7, plot8,
                                                                   plot9, plot10, plot11, plot12,
                                                                   plot13, plot14, plot15, plot16, layout_matrix = plot_grid))
            if (plots_save == TRUE){
              if (base::is.null(save_filename)){
                ggplot2::ggsave(filename = base::paste0(base::paste(base::Sys.Date(), date_var_name, numeric_var_name), ".png"), plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
                plot_name <- base::paste0(base::paste(base::Sys.Date(), date_var_name, numeric_var_name), ".png")
                base::print(base::paste(base::getwd(), plot_name, sep = "/"))
              } else {
                ggplot2::ggsave(filename = base::paste0(save_filename, ".png"), plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
                plot_name <- base::paste0(save_filename, ".png")
                base::print(base::paste(base::getwd(), plot_name, sep = "/"))
              }
            }
################################################################################       
      } else {
        base::print("ERROR: Type of provided numeric_var is not appropariate (require date)")}
    } else {
      base::print("ERROR: Type of provided date_var is not appropariate (require numeric or integer)")}
  } else {
    base::print("ERROR: Type of provided data is not appropariate (require tibble or dataframe)")}
}

if (!require(nycflights13)){utils::install.packages('nycflights13'); require('nycflights13')}
library(nycflights13)

flights_2013 <- flights
flights_2014 <- flights %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::sample_frac(size = 1.25, replace = TRUE)
flights_2015 <- flights %>%
  dplyr::mutate(year = year + 2) %>%
  dplyr::sample_frac(size = 1.5, replace = TRUE)
dplyr::bind_rows(flights_2013, flights_2014, flights_2015) %>%
  dplyr::mutate(date = lubridate::make_date(year, month, day)) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(flights = dplyr::n()) %>%
  dplyr::ungroup() %>%
  plots_date_numeric(data = ., 
                   date_var = date, date_axis = "Date",
                   numeric_var = flights, numeric_axis = "Flights",
                   span = 201)

