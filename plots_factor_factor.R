# Deep visual analysis factor vs factor:
plots_factor_factor <- function(data, factor_var_1, factor_var_2,
                                
                                # Sample data from original datatset to make faster function compilation:
                                data_size = 1.0, seed_value = 42,
                                
                                # Axis, labels:
                                factor_axis_1 = NULL, factor_axis_2 = NULL,  
                                count_axis = "COUNT", percentage_axis = "PERCENTAGE", density_axis = "DENSITY", caption = NULL,
                                
                                # Charts titles:
                                title_1 = "QUANTITY BAR PLOT", 
                                title_2 = "PERCENTAGE BAR PLOT",
                                title_3 = "WAFFLE CHART",
                                title_4 = "CIRCLE PACKING PLOT",
                                title_5 = "QUANTITY TILE PLOT",
                                title_6 = "SCALED PERCENTAGE TILE PLOT",
                                title_7 = "SCALED PERCENTAGE TILE PLOT",
                                title_8 = "PARALLEL PLOT",
                                title_9 = "PERCENTAGE TILE PLOT", 
                                title_10 = "PROPORTION PERCENTAGE BAR PLOT",
                                title_11 = "PROPORTION PERCENTAGE BAR PLOT",
                                title_12 = "QUANTITY BAR PLOT",
                                title_13 = "PERCENTAGE BAR PLOT",
                                title_14 = "WAFFLE CHART",
                                title_15 = "CIRCLE PACKING PLOT",
                                
                                # Additional parameters:
                                text_size = 7, title_size = 9,
                                
                                # Plot save parameters:
                                plots_save = FALSE, save_filename = NULL, save_width = 40, save_height = 40, save_dpi = 500,
                                
                                # Grid display parameters:
                                plot_grid = base::matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 8, 12, 13, 14, 15), byrow = TRUE, ncol = 4)
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
  if (!require(ggparallel)){utils::install.packages('ggparallel'); require('ggparallel')}  
  
  # Additional options:
  base::options(scipen = 20)
  
  # Variables:
  factor_var_1 <- dplyr::enquo(factor_var_1)                    
  factor_var_2  <- dplyr::enquo(factor_var_2) 
  factor_var_1_name <- dplyr::quo_name(factor_var_1)
  factor_var_2_name <- dplyr::quo_name(factor_var_2)
  
  # Conditions:
  if (base::is.data.frame(data) | tibble::is_tibble(data)){
    base::print("INFO: Type of provided data is appropriate")
    if (base::is.factor(data[[base::which(base::names(data) == factor_var_1_name)]])){
      base::print("INFO: Type of provided factor_var_1 is appropariate")
      if (base::is.factor(data[[base::which(base::names(data) == factor_var_2_name)]])){
        base::print("INFO: Type of provided factor_var_2 is appropariate")
        if (base::is.null(factor_axis_1)){factor_axis_1 <- "FACTOR VARIABLE 1"} else {factor_axis_1 <- stringr::str_to_upper(factor_axis_1)}
        if (base::is.null(factor_axis_2)){factor_axis_2 <- "FACTOR VARIABLE 2"} else {factor_axis_2 <- stringr::str_to_upper(factor_axis_2)}
        if (base::is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(base::paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- dplyr::as_tibble(data)
        base::set.seed(seed = seed_value)
        data %>% dplyr::sample_frac(size = data_size) -> data
################################################################################          
          # PLOT 1:
          data %>%
            dplyr::select(!!factor_var_1) %>%
            dplyr::group_by(!!factor_var_1) %>%
            dplyr::summarise(count = dplyr::n(), percentage = dplyr::n()/base::nrow(.)) -> data_cut
          data %>%
            dplyr::select(!!factor_var_1) -> var; var <- c(var); var <- base::unlist(var)
          mean_value <- base::length(var)/base::length(base::unique(data[[base::which(base::names(data) == factor_var_1_name)]]))
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var_1, y = count, label = count, fill = !!factor_var_1)) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis_1, y = count_axis, title = title_1) +
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
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) +
            ggplot2::scale_y_continuous(limits = c(0, 1.1 * base::max(data_cut$count))) -> plot1
################################################################################          
          # PLOT 2:
          data %>% dplyr::select(!!factor_var_1) -> var; var <- c(var); var <- base::unlist(var)
          mean_value <- (base::length(var)/base::length(base::unique(data[[base::which(base::names(data) == factor_var_1_name)]])))/base::length(var)
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var_1, y = percentage, fill = !!factor_var_1, label = paste(100 * round(percentage, 4), "%", sep = ""))) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis_1, y = percentage_axis, title = title_2) +
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
            ggplot2::scale_y_continuous(limits = c(0, 1.1 * max(data_cut$percentage)), labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"), breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) -> plot2
################################################################################
          # PLOT 3:
          grid_size = 50
          data %>% dplyr::select(!!factor_var_1) -> var
          df <- base::expand.grid(y = 1:grid_size, x = 1:grid_size)
          categ_table <- base::round(table(var) * ((grid_size*grid_size)/base::nrow(var)))
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
          df$var <- base::factor(base::rep(base::names(categ_table), categ_table), levels = levels(data[[base::which(base::names(data) == factor_var_1_name)]]), ordered = TRUE)
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
            ggplot2::guides(fill = guide_legend(paste(factor_axis_1, ":", sep = ""), ncol = 1)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) -> plot3
################################################################################          
          # PLOT 4:
          circle_data <- data %>%
            dplyr::select(!!factor_var_1) %>%
            dplyr::arrange(dplyr::desc(!!factor_var_1)) %>%
            dplyr::group_by(!!factor_var_1) %>%
            dplyr::count() %>%
            dplyr::ungroup() %>%
            dplyr::mutate(prop = 100 * n/base::sum(n, na.rm = TRUE)); base::as.data.frame(circle_data) -> circle_data
          packing <- packcircles::circleProgressiveLayout(circle_data$n, sizetype = 'area')
          data_packing = base::cbind(circle_data, packing)
          ggplot_circle_data <- packcircles::circleLayoutVertices(packing, npoints = 10000)
          ggplot2::ggplot() +
            ggplot2::geom_polygon(data = ggplot_circle_data, aes(x, y, group = id, fill = as.factor(id)), colour = "black") +
            ggplot2::geom_label(data = data_packing, aes(x = x, y = y, label = !!factor_var_1), color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_size_continuous(range = c(1,4)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) +
            ggplot2::labs(title = title_4, caption = "") +
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
################################################################################   
          # PLOT 5:
          data %>%
            dplyr::select(!!factor_var_1, !!factor_var_2) %>%
            dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
            dplyr::count() %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = base::list(n = 0)) %>%
            ggplot2::ggplot(data = ., mapping = aes(x = !!factor_var_2, y = !!factor_var_1, fill = n, label = n)) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(aes(x = !!factor_var_2, y = !!factor_var_1), color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
            ggplot2::labs(x = factor_axis_2, y = factor_axis_1, title = title_5) +
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
                           legend.position = "none") -> plot5
################################################################################          
          # plot 6:
          data %>%
            dplyr::select(!!factor_var_1) %>%
            dplyr::group_by(!!factor_var_1) %>%
            dplyr::mutate(count_2 = n()) %>%
            dplyr::distinct() -> t1
          data %>%
            dplyr::select(!!factor_var_1, !!factor_var_2) %>%
            dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
            dplyr::mutate(count_1 = dplyr::n()) %>%
            dplyr::distinct() %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = base::list(count_1 = 0)) %>%
            dplyr::left_join(t1, by = factor_var_1_name) %>%
            dplyr::mutate(percent = base::round(100 * count_1/count_2, 2)) %>%
            dplyr::select(!!factor_var_1, !!factor_var_2, percent) %>%
            ggplot2::ggplot(mapping = aes(x = !!factor_var_1, y = !!factor_var_2, fill = percent, label = base::paste0(percent, "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(aes(x = !!factor_var_1, y = !!factor_var_2), color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
            ggplot2::labs(x = factor_axis_1, y = factor_axis_2, title = title_6) +
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
                           legend.position = "none") -> plot6
################################################################################     
          # plot 7:
          data %>%
            dplyr::select(!!factor_var_2) %>%
            dplyr::group_by(!!factor_var_2) %>%
            dplyr::mutate(count_2 = dplyr::n()) %>%
            dplyr::distinct() -> t1
          data %>%
            dplyr::select(!!factor_var_2, !!factor_var_1) %>%
            dplyr::group_by(!!factor_var_2, !!factor_var_1) %>%
            dplyr::mutate(count_1 = dplyr::n()) %>%
            dplyr::distinct() %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var_2, !!factor_var_1, fill = base::list(count_1 = 0)) %>%
            dplyr::left_join(t1, by = factor_var_2_name) %>%
            dplyr::mutate(percent = base::round(100 * count_1/count_2, 2)) %>%
            dplyr::select(!!factor_var_2, !!factor_var_1, percent) %>%
            ggplot2::ggplot(mapping = aes(x = !!factor_var_2, y = !!factor_var_1, fill = percent, label = base::paste0(percent, "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(aes(x = !!factor_var_2, y = !!factor_var_1), color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
            ggplot2::labs(x = factor_axis_2, y = factor_axis_1, title = title_7) +
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
                           legend.position = "none") -> plot7
################################################################################           
          # PLOT 8:
          data_plot <- data
          data_plot %>%
            dplyr::select(!!factor_var_1, !!factor_var_2) %>%
            dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
            dplyr::count() %>%
            dplyr::ungroup() %>%
            dplyr::select(!!factor_var_1, !!factor_var_2, n) %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = base::list(n = 0)) -> data_plot
          data_plot <- base::as.data.frame(data_plot)
          ggparallel::ggparallel(vars = base::list(base::names(data_plot)[c(base::which(base::names(data_plot) == factor_var_1_name), base::which(base::names(data_plot) == factor_var_2_name))]), data = data_plot, weight = "n", method = "parset", text.angle = 0, label = FALSE, order = 0) +
            ggplot2::scale_fill_manual(values = c(rep("gray60", base::length(base::unique(data_plot[[base::which(base::names(data_plot) == factor_var_2_name)]])) + length(unique(data_plot[[which(names(data_plot) == factor_var_1_name)]]))))) +
            ggplot2::scale_colour_manual(values = c(rep("gray50", base::length(base::unique(data_plot[[base::which(base::names(data_plot) == factor_var_2_name)]]))),
                                                    rep("gray50", base::length(base::unique(data_plot[[base::which(base::names(data_plot) == factor_var_1_name)]]))))) -> p1
          abc <- ggplot2::ggplot_build(p1)$data[[2]]
          tibble::as_tibble(abc) %>%
            dplyr::select(ymin, ymax, xmin, xmax) %>%
            dplyr::mutate(x = (xmin + xmax)/2, y = (ymin + ymax)/2, label = c(base::levels(data[[base::which(base::names(data) == factor_var_1_name)]]), base::levels(data[[base::which(base::names(data) == factor_var_2_name)]]))) -> abc
          p1 +
            geom_label(data = abc, mapping = aes(x = x, y = y, label = label), color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
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
                           legend.position = "none") +
            ggplot2::labs(x = "FACTOR VARIABLES", y = count_axis, title = title_8) -> plot8
################################################################################            
          # PLOT 9:
          data %>%
            dplyr::select(!!factor_var_1, !!factor_var_2) %>%
            dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
            dplyr::count() %>%
            dplyr::mutate(percent = 100 * n/base::nrow(data)) %>%
            dplyr::ungroup() %>%
            dplyr::select(!!factor_var_1, !!factor_var_2, percent) %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = base::list(percent = 0)) %>%
            ggplot2::ggplot(data = ., mapping = aes(x = !!factor_var_2, y = !!factor_var_1, fill = percent, label = base::paste0(base::round(percent, 2), "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(aes(x = !!factor_var_2, y = !!factor_var_1), color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
            ggplot2::labs(x = factor_axis_2, y = factor_axis_1, title = title_9) +
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
                           legend.position = "none") -> plot9
################################################################################
          # PLOT 10:
          ggplot2::ggplot(data = data, mapping = aes(x = !!factor_var_1, fill = !!factor_var_2)) +
            ggplot2::geom_bar(position = "fill", color = "black") +
            ggplot2::labs(x = factor_axis_1, y = percentage_axis, title = title_10) +
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
                           legend.key = element_rect(colour = "gray90")) +
            ggplot2::scale_y_continuous(limits = c(0, 1), labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"), breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
            ggplot2::guides(fill = guide_legend(paste(factor_axis_2, ":", sep = ""), ncol = 1)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) -> plot10
################################################################################          
          # PLOT 11:
          ggplot2::ggplot(data = data, mapping = aes(x = !!factor_var_2, fill = !!factor_var_1)) +
            ggplot2::geom_bar(position = "fill", color = "black") +
            ggplot2::labs(x = factor_axis_2, y = percentage_axis, title = title_11) +
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
                           legend.key = element_rect(colour = "gray90")) +
            ggplot2::scale_y_continuous(limits = c(0, 1), labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"), breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
            ggplot2::guides(fill = guide_legend(paste(factor_axis_1, ":", sep = ""), ncol = 1)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) -> plot11
################################################################################           
          # PLOT 12:
          data %>%
            dplyr::select(!!factor_var_2) %>%
            dplyr::group_by(!!factor_var_2) %>%
            dplyr::summarise(count = dplyr::n(), percentage = n()/base::nrow(.)) -> data_cut
          data %>%
            dplyr::select(!!factor_var_2) -> var; var <- c(var); var <- base::unlist(var)
          mean_value <- base::length(var)/base::length(base::unique(data[[base::which(base::names(data) == factor_var_2_name)]]))
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var_2, y = count, label = count, fill = !!factor_var_2)) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis_1, y = count_axis, title = title_12) +
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
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) +
            ggplot2::scale_y_continuous(limits = c(0, 1.1 * base::max(data_cut$count))) -> plot12
################################################################################          
          # PLOT 13:
          data %>% dplyr::select(!!factor_var_2) -> var; var <- c(var); var <- base::unlist(var)
          mean_value <- (base::length(var)/base::length(base::unique(data[[base::which(base::names(data) == factor_var_2_name)]])))/base::length(var)
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var_2, y = percentage, fill = !!factor_var_2, label = paste(100 * round(percentage, 4), "%", sep = ""))) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 0.5, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis_1, y = percentage_axis, title = title_13) +
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
            ggplot2::scale_y_continuous(limits = c(0, 1.1 * base::max(data_cut$percentage)), labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"), breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) -> plot13
################################################################################         
          # PLOT 14:
          grid_size = 50
          data %>% select(!!factor_var_2) -> var
          df <- base::expand.grid(y = 1:grid_size, x = 1:grid_size)
          categ_table <- base::round(base::table(var) * ((grid_size*grid_size)/base::nrow(var)))
          if(base::sum(categ_table) == grid_size * grid_size){
            categ_table <- categ_table
          } else if (base::sum(categ_table) > grid_size * grid_size){
            los = base::factor(base::rep(base::names(categ_table), categ_table))
            los = base::sample(los, grid_size * grid_size, replace = FALSE)
            categ_table <- base::table(los)
          } else {
            los =  grid_size * grid_size - base::sum(categ_table)
            categ_table[base::which.max(categ_table)] <- categ_table[base::which.max(categ_table)] + los}
          df$var <- base::factor(base::rep(base::names(categ_table), categ_table), levels = base::levels(data[[base::which(base::names(data) == factor_var_2_name)]]), ordered = TRUE)
          ggplot2::ggplot(df, aes(x = x, y = y, fill = var)) +
            ggplot2::geom_tile(color = "black") +
            ggplot2::scale_x_continuous(expand = c(0, 0)) +
            ggplot2::scale_y_continuous(expand = c(0, 0), trans = "reverse") + #, trans = "reverse"
            ggplot2::labs(title = title_14) +
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
            ggplot2::guides(fill = guide_legend(base::paste(factor_axis_2, ":", sep = ""), ncol = 1)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) -> plot14
################################################################################            
          # PLOT 15:
          circle_data <- data %>%
            dplyr::select(!!factor_var_2) %>%
            dplyr::arrange(desc(!!factor_var_2)) %>%
            dplyr::group_by(!!factor_var_2) %>%
            dplyr::count() %>%
            dplyr::ungroup() %>%
            dplyr::mutate(prop = 100 * n/base::sum(n, na.rm = TRUE)); base::as.data.frame(circle_data) -> circle_data
          packing <- packcircles::circleProgressiveLayout(circle_data$n, sizetype='area')
          data_packing = base::cbind(circle_data, packing)
          ggplot_circle_data <- packcircles::circleLayoutVertices(packing, npoints = 10000)
          ggplot2::ggplot() +
            ggplot2::geom_polygon(data = ggplot_circle_data, aes(x, y, group = id, fill = base::as.factor(id)), colour = "black") +
            ggplot2::geom_label(data = data_packing, aes(x = x, y = y, label = !!factor_var_2), color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_size_continuous(range = c(1,4)) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(base::length(base::unique(data[[base::which(base::names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) +
            ggplot2::labs(title = title_15,  caption = caption) +
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
                           legend.position = "none") -> plot15
################################################################################             
          grDevices::dev.new()
          plots <- gridExtra::grid.arrange(gridExtra::arrangeGrob(plot1, plot2, plot3, plot4,
                                                                  plot5, plot6, plot7, plot8,
                                                                  plot9, plot10, plot11, plot12,
                                                                  plot13, plot14, plot15, layout_matrix = plot_grid))
          if (plots_save == TRUE){
            if (base::is.null(save_filename)){
              ggplot2::ggsave(filename = base::paste0(base::paste(base::Sys.Date(), factor_var_1_name, factor_var_2_name), ".png"), plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
              plot_name <- base::paste0(base::paste(base::Sys.Date(), factor_var_1_name, factor_var_2_name), ".png")
              base::print(base::paste(base::getwd(), plot_name, sep = "/"))
            } else {
              ggplot2::ggsave(filename = base::paste0(save_filename, ".png"), plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
              plot_name <- base::paste0(save_filename, ".png")
              base::print(base::paste(base::getwd(), plot_name, sep = "/"))
            }
          }
################################################################################  
      } else {
        base::print("ERROR: Type of provided factor_var_2 is not appropariate (require factor)")}
    } else {
      base::print("ERROR: Type of provided factor_var_1 is not appropariate (require factor)")}
  } else {
    base::print("ERROR: Type of provided data is not appropariate (require tibble or dataframe)")}
}

plots_factor_factor(data = diamonds,
                    data_size = 1.00, 
                    factor_var_1 = cut, factor_axis_1 = "CUT",
                    factor_var_2 = clarity, factor_axis_2 = "CLARITY",
                    caption = "Diamonds (ggplot2 dataset)")

