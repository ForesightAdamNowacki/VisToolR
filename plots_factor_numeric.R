# Deep visual analysis factor vs numeric:
plots_factor_numeric <- function(data, factor_var, numeric_var,  
                                 
                                 # Sample data from original datatset to make faster function compilation:
                                 data_size = 0.5, seed_value = 42,
                                 
                                 # Axis, labels:
                                 factor_axis = NULL, numeric_axis = NULL,  count_axis = "COUNT", percentage_axis = "PERCENTAGE", density_axis = "DENSITY", caption = NULL,
                                 
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
                                 text_size = 7, title_size = 9, histogram_bins = 10, numeric_cuts = 10,

                                 # Plot save parameters:
                                 plots_save = FALSE, save_filename = NULL, save_width = 40, save_height = 40, save_dpi = 1000,
                                 
                                 # Grid display parameters:
                                 plot_grid = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), byrow = TRUE, ncol = 4)
                                 
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
  numeric_var <- dplyr::enquo(numeric_var)                    
  factor_var  <- dplyr::enquo(factor_var) 
  numeric_var_name <- dplyr::quo_name(numeric_var)
  factor_var_name <- dplyr::quo_name(factor_var)
  
  # Conditions:
  if (base::is.data.frame(data) | tibble::is_tibble(data)){
    base::print("INFO: Type of provided data is appropriate")
    if (base::is.factor(data[[base::which(base::names(data) == factor_var_name)]])){
      base::print("INFO: Type of provided factor_var is appropariate")
      if (base::is.numeric(data[[base::which(base::names(data) == numeric_var_name)]]) | base::is.integer(data[[base::which(base::names(data) == numeric_var_name)]])){
        base::print("INFO: Type of provided numeric_var is appropariate")
        if (base::is.null(factor_axis)){factor_axis <- "FACTOR VARIABLE"} else {factor_axis <- stringr::str_to_upper(factor_axis)}
        if (base::is.null(numeric_axis)){numeric_axis <- "NUMERIC VARIABLE"} else {numeric_axis <- stringr::str_to_upper(numeric_axis)}
        if (base::is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- dplyr::as_tibble(data)
        base::set.seed(seed = seed_value)
        data %>% dplyr::sample_frac(size = data_size) -> data
################################################################################         
          # PLOT 1:
          data %>% 
            dplyr::select(!!factor_var) %>% 
            dplyr::group_by(!!factor_var) %>% 
            dplyr::summarise(count = dplyr::n(), percentage = dplyr::n()/base::nrow(.)) -> data_cut
          data %>% 
            dplyr::select(!!factor_var) -> var; var <- c(var); var <- unlist(var)
          mean_value <- length(var)/length(unique(data[[which(names(data) == factor_var_name)]]))
          
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var, y = count, label = count, fill = !!factor_var)) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis,
                          y = count_axis,
                          title = title_1) +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
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
                           legend.position = "none") +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) +
            ggplot2::scale_y_continuous(limits = c(0, 1.1 * max(data_cut$count))) -> plot1
          
          # PLOT 2:  
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var)
          mean_value <- (length(var)/length(unique(data[[which(names(data) == factor_var_name)]])))/length(var)
          
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var, y = percentage, fill = !!factor_var, label = paste(100 * round(percentage, 4), "%", sep = ""))) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis,
                          y = percentage_axis,
                          title = title_2) +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
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
                           legend.position = "none") +
            ggplot2::scale_y_continuous(limits = c(0, 1.1 * max(data_cut$percentage)),
                                        labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
                                        breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot2
          
          # PLOT 3:  
          grid_size = 50
          data %>% select(!!factor_var) -> var
          df <- expand.grid(y = 1:grid_size, x = 1:grid_size)
          categ_table <- round(table(var) * ((grid_size*grid_size)/nrow(var)))
          if(sum(categ_table) == grid_size * grid_size){
            categ_table <- categ_table
          } else if (sum(categ_table) > grid_size * grid_size){
            los = factor(rep(names(categ_table), categ_table))
            los = sample(los, grid_size * grid_size, replace = FALSE)
            categ_table <- table(los)
          } else {
            los =  grid_size * grid_size - sum(categ_table)
            categ_table[which.max(categ_table)] <- categ_table[which.max(categ_table)] + los
          }
          df$var <- factor(rep(names(categ_table), categ_table), levels = levels(data[[which(names(data) == factor_var_name)]]), ordered = TRUE)
          
          ggplot2::ggplot(df, aes(x = x, y = y, fill = var)) +
            ggplot2::geom_tile(color = "black") +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0), trans = "reverse") + #, trans = "reverse"
            ggplot2::labs(title = title_3) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.title.x = element_blank(),
                           axis.text.x = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.title.y = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks.y = element_blank(),
                           plot.background = element_rect(fill = "gray80", color = "black", size = 1, linetype = "solid"),
                           panel.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
                           panel.border = element_rect(fill = NA, color = "black", size = 0.5, linetype = "solid"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.box.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
                           legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                           legend.position = "right",
                           legend.box.spacing = unit(0.25, "cm"),
                           legend.text = element_text(size = text_size, color = "black", face = "plain"),
                           legend.title = element_text(size = text_size, color = "black", face = "bold")) +
            ggplot2::guides(fill = guide_legend(paste(factor_axis, ":", sep = ""), ncol = 1)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot3
          
          # PLOT 4
          circle_data <- data %>%
            select(!!factor_var) %>%
            arrange(desc(!!factor_var)) %>%
            group_by(!!factor_var) %>%
            count() %>%
            ungroup() %>%
            mutate(prop = 100 * n/sum(n, na.rm = TRUE)); as.data.frame(circle_data) -> circle_data
          packing <- circleProgressiveLayout(circle_data$n, sizetype='area')
          data_packing = cbind(circle_data, packing)
          ggplot_circle_data <- circleLayoutVertices(packing, npoints = 10000)
          
          ggplot2::ggplot() + 
            ggplot2::geom_polygon(data = ggplot_circle_data, aes(x, y, group = id, fill = as.factor(id)), colour = "black") +
            ggplot2::geom_label(data = data_packing, aes(x = x, y = y, label = !!factor_var),
                                color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_size_continuous(range = c(1,4)) +
            # ggplot2::scale_fill_manual(values = scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) +
            ggplot2::labs(title = title_4) +
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
                           legend.position = "none") -> plot4
          
          # PLOT 5:  
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); mean_value <- mean(var, na.rm = TRUE)
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); median_value <- median(var, na.rm = TRUE)
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); Q1_value <- quantile(var, 0.25, na.rm = TRUE)
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); Q3_value <- quantile(var, 0.75, na.rm = TRUE)
          
          ggplot2::ggplot(data = data, aes(x = !!factor_var, y = !!numeric_var, fill = !!factor_var)) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 1, lwd = 0.5, color = "black") +
            ggplot2::geom_hline(yintercept = median_value, lty = 2, lwd = 0.5, color = "black") +    
            ggplot2::geom_hline(yintercept = Q1_value, lty = 2, lwd = 0.5, color = "black") +   
            ggplot2::geom_hline(yintercept = Q3_value, lty = 2, lwd = 0.5, color = "black") +   
            ggplot2::geom_boxplot(color = "black", outlier.color = "black", notch = FALSE) + 
            ggplot2::stat_summary(fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::labs(x = factor_axis,
                          y = numeric_axis,
                          title = title_5) +
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
                           legend.position = "none") +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot5
          
          # PLOT 6:  
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); mean_value <- mean(var, na.rm = TRUE)
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); median_value <- median(var, na.rm = TRUE)
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); Q1_value <- quantile(var, 0.25, na.rm = TRUE)
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); Q3_value <- quantile(var, 0.75, na.rm = TRUE)
          
          ggplot2::ggplot(data = data, aes(x = !!factor_var, y = !!numeric_var, fill = !!factor_var)) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 1, lwd = 0.5, color = "black") +
            ggplot2::geom_hline(yintercept = median_value, lty = 2, lwd = 0.5, color = "black") +    
            ggplot2::geom_hline(yintercept = Q1_value, lty = 2, lwd = 0.5, color = "black") +   
            ggplot2::geom_hline(yintercept = Q3_value, lty = 2, lwd = 0.5, color = "black") +   
            ggplot2::geom_violin(draw_quantiles = c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", trim = FALSE) +
            ggplot2::stat_summary(fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::labs(x = factor_axis,
                          y = numeric_axis,
                          title = title_6) +
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
                           legend.position = "none") +
            # ggplot2::scale_fill_manual(values = scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) 
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot6
          
          # PLOT 7:
          ggplot2::ggplot(data, aes(x = !!numeric_var, y = !!factor_var, fill = !!factor_var)) +
            ggridges::geom_density_ridges2(scale = 0.95, quantile_lines = TRUE, rel_min_height = 0.001) + 
            ggplot2::labs(x = numeric_axis,
                          y = factor_axis,
                          title = title_7) +
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
            ggplot2::guides(fill = guide_legend(paste(factor_axis, ":", sep = ""), nrow = 1)) +
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) +
            ggplot2::scale_y_discrete(expand = expand_scale(mult = 0.05)) -> plot7
          
          # PLOT 8:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var)) +
            ggplot2::geom_density(aes(fill = !!factor_var), position= "stack") +
            ggplot2::labs(x = numeric_axis,
                          y = percentage_axis,
                          title = title_8) +
            ggplot2::theme(plot.title = element_text(size = text_size, color = "black", face = "bold", hjust = 0.5, vjust = 0.5),
                           axis.text.y = element_text(size = text_size, color = "black", face = "plain"),
                           axis.text.x = element_text(size = text_size, color = "black", face = "plain"),
                           axis.title.y = element_text(size = text_size, color = "black", face = "bold"),
                           axis.title.x = element_text(size = text_size, color = "black", face = "bold"),  
                           axis.ticks = element_line(size = 1, color = "black", linetype = "solid"),
                           axis.ticks.length = unit(0.25, "cm"),
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
                           legend.box.spacing = unit(0.1, "cm"),
                           legend.text = element_text(size = text_size, color = "black", face = "plain"),
                           legend.title = element_text(size = text_size, color = "black", face = "bold"),
                           legend.key = element_rect(color = "gray90")) +
            ggplot2::guides(fill = guide_legend(paste(factor_axis, ":", sep = ""), ncol = 1)) +
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) 
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot8
          
          # PLOT 9:
          data %>%
            mutate(mean_value = mean(!!numeric_var, na.rm = TRUE)) %>%
            group_by(!!factor_var) %>%
            mutate(mean_group = mean(!!numeric_var, na.rm = TRUE)) %>%
            select(!!factor_var, mean_value, mean_group) %>%
            distinct() %>%
            mutate(type = ifelse(mean_group < mean_value, "Mean below average", "Mean above average")) %>%
            arrange(mean_group) %>%
            mutate(mean_group = mean_group - mean_value) %>%
            ungroup() -> data_chart
          
          ggplot(data = data_chart, aes(x = !!factor_var, y = mean_group, label = round(mean_group, 2))) +
            ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 1 * sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 1 * -sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 2 * sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 2 * -sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 3 * sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 3 * -sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
            ggplot2::scale_fill_manual(name = "GROUP:",
                                       labels = c("MEAN ABOVE AVERAGE", "MEAN BELOW AVERAGE"),
                                       values = c("Mean above average" = "grey30",
                                                  "Mean below average" = "grey50")) +
            ggplot2::labs(x = factor_axis,
                          y = "DIFFERENCE",
                          title = title_9) +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
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
          
          # PLOT 10:
          data %>%
            mutate(median_value = median(!!numeric_var, na.rm = TRUE)) %>%
            group_by(!!factor_var) %>%
            mutate(median_group = median(!!numeric_var, na.rm = TRUE)) %>%
            ungroup() %>%          
            select(!!factor_var, median_value, median_group) %>%
            distinct() %>%
            mutate(type = ifelse(median_group < median_value, "Median below average", "Median above average")) %>%
            arrange(median_group) %>%
            mutate(median_group = median_group - median_value) %>%
            ungroup() -> data_chart
          
          ggplot(data = data_chart, aes(x = !!factor_var, y = median_group, label = round(median_group, 2))) +
            ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 1 * sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 1 * -sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 2 * sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 2 * -sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 3 * sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_hline(yintercept = 3 * -sd(data_chart$median_group), lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
            ggplot2::scale_fill_manual(name = "GROUP:",
                                       labels = c("MEDIAN ABOVE AVERAGE", "MEDIAN BELOW AVERAGE"),
                                       values = c("Median above average" = "grey30",
                                                  "Median below average" = "grey50")) +
            ggplot2::labs(x = factor_axis,
                          y = "DIFFERENCE",
                          title = title_10) +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
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
            ggplot2::guides(ncol = 1) -> plot10
          
          # PLOT 11:
          data %>% mutate(cuts = cut_number(!!numeric_var, n = numeric_cuts)) -> data
          
          ggplot2::ggplot(data = data, aes(x = cuts, fill = !!factor_var)) +
            ggplot2::geom_bar(position = "fill", color = "black") +  
            ggplot2::labs(x = numeric_axis,
                          y = percentage_axis,
                          title = title_11) +
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
                           panel.grid.major.x = element_line(linetype = "blank"),
                           panel.grid.major.y = element_line(linetype = "blank"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.box.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
                           legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                           legend.position = "right",
                           legend.box.spacing = unit(0.25, "cm"),
                           legend.text = element_text(size = text_size, color = "black", face = "plain"),
                           legend.title = element_text(size = text_size, color = "black", face = "bold"),
                           legend.key = element_rect(color = "gray90")) +
            ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                                        breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) +
            ggplot2::guides(fill = guide_legend(paste(factor_axis, ":", sep = ""), ncol = 1)) +
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) +
            coord_flip() -> plot11
          
          # PLOT 12:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var, color = !!factor_var)) +
            ggplot2::stat_ecdf(lwd = 1) +
            ggplot2::labs(x = numeric_axis,
                          y = percentage_axis,
                          title = title_12) +
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
                           legend.key = element_rect(color = "black", fill = "gray90")) +
            ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                                        breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) +
            ggplot2::guides(color = guide_legend(paste(factor_axis, ":", sep = ""), ncol = 1)) +
            # ggplot2::scale_color_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) 
            ggplot2::scale_color_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot12
          
          # PLOT 13:  
          ggplot2::ggplot(data = data) +
            ggplot2::geom_boxplot(aes(x = 1, y = !!numeric_var), fill = "gray60", color = "black", outlier.color = "black", notch = FALSE) + 
            ggplot2::geom_violin(aes(x = 3, y = !!numeric_var), draw_quantiles = c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", fill = "gray60", trim = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::labs(x = factor_axis,
                          y = numeric_axis,
                          title = title_13) +
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
          
          # PLOT 14:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var)) +
            ggplot2::geom_histogram(color = "black", bins = histogram_bins, fill = "gray60") +  
            ggplot2::geom_freqpoly(bins = histogram_bins, lwd = 1, color = "black") +
            ggplot2::labs(x = numeric_axis,
                          y = count_axis,
                          title = title_14) +
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
          
          # PLOT 15:
          ggplot2::ggplot(data = data, aes(sample = !!numeric_var)) +
            ggplot2::stat_qq() +
            ggplot2::stat_qq_line(lwd = 1, color = "black") +
            ggplot2::labs(x = "THEORETICAL",
                          y = "SAMPLE",
                          title = title_15) +
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
          
          # PLOT 16:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var)) +
            ggplot2::stat_density(fill = "gray60", color = "black", lwd = 0.5) +
            ggplot2::stat_ecdf(lwd = 1, color = "black") +
            ggplot2::labs(x = numeric_axis,
                          y = percentage_axis,
                          title = title_16,
                          caption = caption) +
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
            ggplot2::scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                                        breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) -> plot16
          
          dev.new()
          plots <- gridExtra::grid.arrange(arrangeGrob(plot1, plot2, plot3, plot4,
                                                       plot5, plot6, plot7, plot8,
                                                       plot9, plot10, plot11, plot12,
                                                       plot13, plot14, plot15, plot16, layout_matrix = plot_grid))
          
          
          if (plots_save == TRUE){
            if (is.null(save_filename)){
              ggsave(filename = paste0(paste(Sys.Date(), factor_var_name, numeric_var_name), ".png"),
                     plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
              plot_name <- paste0(paste(Sys.Date(), factor_var_name, numeric_var_name), ".png")
              print(paste(getwd(), plot_name, sep = "/"))
            } else {
              ggsave(filename = paste0(save_filename, ".png"),
                     plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
              plot_name <- paste0(save_filename, ".png")
              print(paste(getwd(), plot_name, sep = "/"))
            }
          }
      } else {
        print("ERROR: Type of provided numeric_var is not appropariate (require numeric or integer)")}
    } else {
      print("ERROR: Type of provided factor_var is not appropariate (require factor)")}
  } else {
    print("ERROR: Type of provided data is not appropariate (require tibble or dataframe)")}
}

plots_factor_numeric(data = diamonds,
                     factor_var = cut, factor_axis = "CUT",
                     numeric_var = carat, numeric_axis = "CARAT",
                     caption = "Diamonds (ggplot2 dataset)", numeric_cuts = 5)

