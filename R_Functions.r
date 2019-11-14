# source("C:\\Users\\adam.nowacki\\Desktop\\My_Files\\R_Functions.R")

# Function to calculate NA proportion in column:
NA_rate <- function(x){sum(is.na(x))/length(x)}

# Split data to train dataset (60%), validation dataset (20%), test dataset (20%):
TrainValidationTestDataPartition <- function(data, p1 = 0.8, p2 = 0.75, seed1 = 1, seed2 = 2){
  set.seed(seed1)
  indeks = caret::createDataPartition(unlist(data[,1], use.names = FALSE), times = 1, p = p1, list = FALSE)
  testdata = data[-indeks,]
  trainvalidationdata = data[indeks,]
  
  set.seed(seed2)
  indeks = caret::createDataPartition(unlist(trainvalidationdata[,1], use.names = FALSE), times = 1, p = p2, list = FALSE)
  traindata = trainvalidationdata[indeks,]
  validationdata = trainvalidationdata[-indeks,]
  
  results = list(traindata = traindata,
                 validationdata = validationdata,
                 testdata = testdata)
  return(results)}

# Verification of TrainValidationTestDataPartition function:
DataPartitionVerification <- function(data){ # argumentem jest wynik dzia³ania funkcji TrainValidationTestDataPartition
  print(paste("Train dataset: n =", nrow(data[[1]]), ", % =", round(100 * nrow(data[[1]])/(nrow(data[[1]]) + nrow(data[[2]]) + nrow(data[[3]])), 2)))
  print(paste("Validation dataset: n =", nrow(data[[2]]), ", % =", round(100 * nrow(data[[2]])/(nrow(data[[1]]) + nrow(data[[2]]) + nrow(data[[3]])), 2)))
  print(paste("Test dataset: n =", nrow(data[[3]]), ", % =", round(100 * nrow(data[[3]])/(nrow(data[[1]]) + nrow(data[[2]]) + nrow(data[[3]])), 2)))}

# Deep visual analysis factor vs factor:
plots_factor_factor <- function(data, factor_var_1, factor_var_2,
                                
                                # Sample data from original datatset to make faster function compilation:
                                data_size = 0.5,
                                seed_value = 42,
                                
                                # Axis, labels:
                                factor_axis_1 = NULL, factor_axis_2 = NULL,  
                                count_axis = "COUNT", 
                                percentage_axis = "PERCENTAGE",
                                density_axis = "DENSITY", caption = NULL,
                                
                                # Titles:
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
                                text_size = 7, title_size = 9, histogram_bins = 8, numeric_cuts = 10,
                                
                                # Print:
                                plots_print = TRUE, stats_print = TRUE,
                                
                                # Stats save:
                                stats_save = FALSE, 
                                
                                # Plot save:
                                plots_save = FALSE, save_filename = NULL, save_width = 40, save_height = 40, save_dpi = 1000,
                                
                                # Plot matrix:
                                plot_grid = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 8, 12, 13, 14, 15), byrow = TRUE, ncol = 4)
                                
){
  # Packages:
  if (!require(ggplot2)){install.packages('ggplot2'); require('ggplot2')}
  if (!require(tidyverse)){install.packages('tidyverse'); require('tidyverse')}
  if (!require(dplyr)){install.packages('dplyr'); require('dplyr')}
  if (!require(magrittr)){install.packages('magrittr'); require('magrittr')} 
  if (!require(gridExtra)){install.packages('gridExtra'); require('gridExtra')}
  if (!require(scales)){install.packages('scales'); require('scales')}
  if (!require(stringr)){install.packages('stringr'); require('stringr')}
  if (!require(ggridges)){install.packages('ggridges'); require('ggridges')}
  if (!require(e1071)){install.packages('e1071'); require('e1071')}  
  if (!require(knitr)){install.packages('knitr'); require('knitr')}  
  if (!require(packcircles)){install.packages('packcircles'); require('packcircles')}  
  if (!require(viridis)){install.packages('viridis'); require('viridis')}  
  if (!require(openxlsx)){install.packages('openxlsx'); require('openxlsx')}    
  if (!require(ggparallel)){install.packages('ggparallel'); require('ggparallel')}  
  
  # Additional options:
  options(scipen = 20)
  
  # Variables:
  factor_var_1 <- enquo(factor_var_1)                    
  factor_var_2  <- enquo(factor_var_2) 
  factor_var_1_name <- quo_name(factor_var_1)
  factor_var_2_name <- quo_name(factor_var_2)
  
  # Conditions:
  if (is.data.frame(data) | tibble::is_tibble(data)){
    print("INFO: Type of provided data is appropriate")
    if (is.factor(data[[which(names(data) == factor_var_1_name)]])){
      print("INFO: Type of provided factor_var_1 is appropariate")
      if (is.factor(data[[which(names(data) == factor_var_2_name)]])){
        print("INFO: Type of provided factor_var_2 is appropariate")
        if (is.null(factor_axis_1)){factor_axis_1 <- "FACTOR VARIABLE 1"} else {factor_axis_1 <- stringr::str_to_upper(factor_axis_1)}
        if (is.null(factor_axis_2)){factor_axis_2 <- "FACTOR VARIABLE 2"} else {factor_axis_2 <- stringr::str_to_upper(factor_axis_2)}
        if (is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- dplyr::as_tibble(data)
        set.seed(seed = seed_value)
        data %>%
          dplyr::sample_frac(size = data_size) -> data
        
        if(plots_print == TRUE){
          
          # PLOT 1:
          data %>%
            dplyr::select(!!factor_var_1) %>%
            dplyr::group_by(!!factor_var_1) %>%
            dplyr::summarise(count = n(),
                             percentage = n()/nrow(.)) -> data_cut
          data %>%
            dplyr::select(!!factor_var_1) -> var; var <- c(var); var <- unlist(var)
          mean_value <- length(var)/length(unique(data[[which(names(data) == factor_var_1_name)]]))
          
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var_1, y = count, label = count, fill = !!factor_var_1)) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 1, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis_1,
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
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_1_name)]])))) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) +
            ggplot2::scale_y_continuous(limits = c(0, 1.1 * max(data_cut$count))) -> plot1

          # PLOT 2:
          data %>% select(!!factor_var_1) -> var; var <- c(var); var <- unlist(var)
          mean_value <- (length(var)/length(unique(data[[which(names(data) == factor_var_1_name)]])))/length(var)
          
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var_1, y = percentage, fill = !!factor_var_1, label = paste(100 * round(percentage, 4), "%", sep = ""))) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 1, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis_1,
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
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_1_name)]])))) 
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) -> plot2
          
          # PLOT 3:
          grid_size = 50
          data %>% select(!!factor_var_1) -> var
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
          df$var <- factor(rep(names(categ_table), categ_table), levels = levels(data[[which(names(data) == factor_var_1_name)]]), ordered = TRUE)
          
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
            # ggplot2::scale_fill_manual(values = scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_1_name)]]))))
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) -> plot3
          
          # PLOT 4
          circle_data <- data %>%
            select(!!factor_var_1) %>%
            arrange(desc(!!factor_var_1)) %>%
            group_by(!!factor_var_1) %>%
            count() %>%
            ungroup() %>%
            mutate(prop = 100 * n/sum(n, na.rm = TRUE)); as.data.frame(circle_data) -> circle_data
          packing <- circleProgressiveLayout(circle_data$n, sizetype='area')
          data_packing = cbind(circle_data, packing)
          ggplot_circle_data <- circleLayoutVertices(packing, npoints = 10000)
          
          ggplot2::ggplot() +
            ggplot2::geom_polygon(data = ggplot_circle_data, aes(x, y, group = id, fill = as.factor(id)), colour = "black") +
            ggplot2::geom_label(data = data_packing, aes(x = x, y = y, label = !!factor_var_1),
                                color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_size_continuous(range = c(1,4)) +
            # ggplot2::scale_fill_manual(values = scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_1_name)]])))) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) +
            ggplot2::labs(title = title_4,
                          caption = "") +
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
          data %>%
            dplyr::select(!!factor_var_1, !!factor_var_2) %>%
            dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
            dplyr::count() %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(n = 0)) %>%
            ggplot2::ggplot(data = ., mapping = aes(x = !!factor_var_2, y = !!factor_var_1, fill = n, label = n)) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(aes(x = !!factor_var_2, y = !!factor_var_1), 
                                color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
            ggplot2::labs(x = factor_axis_2,
                          y = factor_axis_1,
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
                           panel.grid.major.x = element_line(linetype = "blank"),
                           panel.grid.major.y = element_line(linetype = "blank"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") -> plot5
          
          # plot 6:
          data %>%
            select(!!factor_var_1) %>%
            group_by(!!factor_var_1) %>%
            mutate(count_2 = n()) %>%
            distinct() -> t1
          
          data %>%
            select(!!factor_var_1, !!factor_var_2) %>%
            group_by(!!factor_var_1, !!factor_var_2) %>%
            mutate(count_1 = n()) %>%
            distinct() %>%
            ungroup() %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(count_1 = 0)) %>%
            dplyr::left_join(t1, by = factor_var_1_name) %>%
            dplyr::mutate(percent = round(100 * count_1/count_2, 2)) %>%
            dplyr::select(!!factor_var_1, !!factor_var_2, percent) %>%
            ggplot2::ggplot(mapping = aes(x = !!factor_var_1, y = !!factor_var_2, fill = percent, label = paste0(percent, "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(aes(x = !!factor_var_1, y = !!factor_var_2),
                                color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
            ggplot2::labs(x = factor_axis_1,
                          y = factor_axis_2,
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
                           panel.grid.major.x = element_line(linetype = "blank"),
                           panel.grid.major.y = element_line(linetype = "blank"),
                           panel.grid.minor.x = element_line(linetype = "blank"),
                           panel.grid.minor.y = element_line(linetype = "blank"),
                           plot.caption = element_text(size = text_size, color = "black", face = "bold", hjust = 1),
                           legend.position = "none") -> plot6
          
          # plot 7:
          data %>%
            select(!!factor_var_2) %>%
            group_by(!!factor_var_2) %>%
            mutate(count_2 = n()) %>%
            distinct() -> t1
          
          data %>%
            select(!!factor_var_2, !!factor_var_1) %>%
            group_by(!!factor_var_2, !!factor_var_1) %>%
            mutate(count_1 = n()) %>%
            distinct() %>%
            ungroup() %>%
            tidyr::complete(!!factor_var_2, !!factor_var_1, fill = list(count_1 = 0)) %>%
            dplyr::left_join(t1, by = factor_var_2_name) %>%
            dplyr::mutate(percent = round(100 * count_1/count_2, 2)) %>%
            dplyr::select(!!factor_var_2, !!factor_var_1, percent) %>%
            ggplot2::ggplot(mapping = aes(x = !!factor_var_2, y = !!factor_var_1, fill = percent, label = paste0(percent, "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(aes(x = !!factor_var_2, y = !!factor_var_1), 
                                color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
            ggplot2::labs(x = factor_axis_2,
                          y = factor_axis_1,
                          title = title_7) +
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
          
          # PLOT 8:
          data_plot <- data
          data_plot %>%
            dplyr::select(!!factor_var_1, !!factor_var_2) %>%
            dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
            dplyr::count() %>%
            dplyr::ungroup() %>%
            dplyr::select(!!factor_var_1, !!factor_var_2, n) %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(n = 0)) -> data_plot
          data_plot <- as.data.frame(data_plot)
          ggparallel::ggparallel(vars = list(names(data_plot)[c(which(names(data_plot) == factor_var_1_name), which(names(data_plot) == factor_var_2_name))]), data = data_plot, weight = "n",
                                 method = "parset", text.angle = 0, label = FALSE, order = 0) +
            ggplot2::scale_fill_manual(values = c(rep("gray60", length(unique(data_plot[[which(names(data_plot) == factor_var_2_name)]])) + length(unique(data_plot[[which(names(data_plot) == factor_var_1_name)]]))))) +
            ggplot2::scale_colour_manual(values = c(rep("gray50", length(unique(data_plot[[which(names(data_plot) == factor_var_2_name)]]))),
                                                    rep("gray50", length(unique(data_plot[[which(names(data_plot) == factor_var_1_name)]]))))) -> p1
          abc <- ggplot2::ggplot_build(p1)$data[[2]]
          
          tibble::as_tibble(abc) %>%
            dplyr::select(ymin, ymax, xmin, xmax) %>%
            dplyr::mutate(x = (xmin + xmax)/2,
                          y = (ymin + ymax)/2,
                          label = c(levels(data[[which(names(data) == factor_var_1_name)]]), levels(data[[which(names(data) == factor_var_2_name)]]))) -> abc
          p1 + 
            geom_label(data = abc, mapping = aes(x = x, y = y, label = label),
                       color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
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
            ggplot2::labs(x = "FACTOR VARIABLES",
                          y = count_axis,
                          title = title_8) -> plot8
          # PLOT 9:
          data %>%
            dplyr::select(!!factor_var_1, !!factor_var_2) %>%
            dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
            dplyr::count() %>%
            dplyr::mutate(percent = 100 * n/nrow(data)) %>%
            dplyr::ungroup() %>%
            dplyr::select(!!factor_var_1, !!factor_var_2, percent) %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(percent = 0)) %>%
            ggplot2::ggplot(data = ., mapping = aes(x = !!factor_var_2, y = !!factor_var_1, fill = percent, label = paste0(round(percent, 2), "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(aes(x = !!factor_var_2, y = !!factor_var_1),
                                color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_fill_gradientn(colours = c("white", "black"), values = c(0, 1)) +
            ggplot2::labs(x = factor_axis_2,
                          y = factor_axis_1,
                          title = title_9) +
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
          
          # PLOT 10:
          ggplot2::ggplot(data = data, mapping = aes(x = !!factor_var_1, fill = !!factor_var_2)) +
            ggplot2::geom_bar(position = "fill", color = "black") +
            ggplot2::labs(x = factor_axis_1,
                          y = percentage_axis,
                          title = title_10) +
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
            ggplot2::scale_y_continuous(limits = c(0, 1),
                                        labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
                                        breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
            ggplot2::guides(fill = guide_legend(paste(factor_axis_2, ":", sep = ""), ncol = 1)) +
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_2_name)]]))))
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) -> plot10
          
          # PLOT 11:
          ggplot2::ggplot(data = data, mapping = aes(x = !!factor_var_2, fill = !!factor_var_1)) +
            ggplot2::geom_bar(position = "fill", color = "black") +
            ggplot2::labs(x = factor_axis_2,
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
                           legend.key = element_rect(colour = "gray90")) +
            ggplot2::scale_y_continuous(limits = c(0, 1),
                                        labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%"),
                                        breaks = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) +
            ggplot2::guides(fill = guide_legend(paste(factor_axis_1, ":", sep = ""), ncol = 1)) +
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_1_name)]]))))
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_1_name)]])) + 1, "Greys")[-1]) -> plot11
          
          # PLOT 12:
          data %>%
            dplyr::select(!!factor_var_2) %>%
            dplyr::group_by(!!factor_var_2) %>%
            dplyr::summarise(count = n(),
                             percentage = n()/nrow(.)) -> data_cut
          data %>%
            dplyr::select(!!factor_var_2) -> var; var <- c(var); var <- unlist(var)
          mean_value <- length(var)/length(unique(data[[which(names(data) == factor_var_2_name)]]))
          
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var_2, y = count, label = count, fill = !!factor_var_2)) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 1, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis_1,
                          y = count_axis,
                          title = title_12) +
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
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_2_name)]])))) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) +
            ggplot2::scale_y_continuous(limits = c(0, 1.1 * max(data_cut$count))) -> plot12
          
          # PLOT 13:
          data %>% select(!!factor_var_2) -> var; var <- c(var); var <- unlist(var)
          mean_value <- (length(var)/length(unique(data[[which(names(data) == factor_var_2_name)]])))/length(var)
          
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var_2, y = percentage, fill = !!factor_var_2, label = paste(100 * round(percentage, 4), "%", sep = ""))) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 1, col = "black") +
            ggplot2::geom_bar(stat = "identity", position = "identity", color = "black") +
            ggplot2::labs(x = factor_axis_1,
                          y = percentage_axis,
                          title = title_13) +
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
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_2_name)]])))) 
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) -> plot13
          
          # PLOT 14:
          grid_size = 50
          data %>% select(!!factor_var_2) -> var
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
          df$var <- factor(rep(names(categ_table), categ_table), levels = levels(data[[which(names(data) == factor_var_2_name)]]), ordered = TRUE)
          
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
            ggplot2::guides(fill = guide_legend(paste(factor_axis_2, ":", sep = ""), ncol = 1)) +
            # ggplot2::scale_fill_manual(values = scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_2_name)]])))) 
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) -> plot14
          
          # PLOT 15:
          circle_data <- data %>%
            select(!!factor_var_2) %>%
            arrange(desc(!!factor_var_2)) %>%
            group_by(!!factor_var_2) %>%
            count() %>%
            ungroup() %>%
            mutate(prop = 100 * n/sum(n, na.rm = TRUE)); as.data.frame(circle_data) -> circle_data
          packing <- circleProgressiveLayout(circle_data$n, sizetype='area')
          data_packing = cbind(circle_data, packing)
          ggplot_circle_data <- circleLayoutVertices(packing, npoints = 10000)
          
          ggplot2::ggplot() +
            ggplot2::geom_polygon(data = ggplot_circle_data, aes(x, y, group = id, fill = as.factor(id)), colour = "black") +
            ggplot2::geom_label(data = data_packing, aes(x = x, y = y, label = !!factor_var_2), 
                                color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::scale_size_continuous(range = c(1,4)) +
            # ggplot2::scale_fill_manual(values = scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_2_name)]])))) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_2_name)]])) + 1, "Greys")[-1]) +
            ggplot2::labs(title = title_15,
                          caption = caption) +
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
          
          dev.new()
          plots <- gridExtra::grid.arrange(arrangeGrob(plot1, plot2, plot3, plot4,
                                                       plot5, plot6, plot7, plot8,
                                                       plot9, plot10, plot11, plot12,
                                                       plot13, plot14, plot15, layout_matrix = plot_grid))
          
          if (plots_save == TRUE){
            if (is.null(save_filename)){
              ggsave(filename = paste0(paste(Sys.Date(), factor_var_1_name, factor_var_2_name), ".png"),
                     plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
              plot_name <- paste0(paste(Sys.Date(), factor_var_1_name, factor_var_2_name), ".png")
              print(paste(getwd(), plot_name, sep = "/"))
            } else {
              ggsave(filename = paste0(save_filename, ".png"),
                     plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
              plot_name <- paste0(save_filename, ".png")
              print(paste(getwd(), plot_name, sep = "/"))
            }
          }
        }
        
        if(stats_print == TRUE){
          
          data %>%
            dplyr::select(!!factor_var_1) %>%
            dplyr::group_by(!!factor_var_1) %>%
            dplyr::mutate(COUNT = n(),
                          PERCENT = round(100 * n()/nrow(data), 2)) %>%
            dplyr::distinct() %>%
            dplyr::arrange(!!factor_var_1) -> t0_1 -> output1
          
          data %>%
            dplyr::select(!!factor_var_2) %>%
            dplyr::group_by(!!factor_var_2) %>%
            dplyr::mutate(COUNT = n(),
                          PERCENT = round(100 * n()/nrow(data), 2)) %>%
            dplyr::distinct() %>%
            dplyr::arrange(!!factor_var_2) -> t0_2 -> output2
          
          data %>%
            dplyr::select(!!factor_var_1, !!factor_var_2) %>%
            dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
            dplyr::mutate(COUNT = n(),
                          PERCENT = round(100 * n()/nrow(data), 2)) %>%
            dplyr::distinct() %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(COUNT = 0, PERCENT = 0)) %>%
            dplyr::mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
                          PERCENT = ifelse(is.na(PERCENT), 0, PERCENT)) -> t1
          
          t1 %>%
            dplyr::select(!!factor_var_1, !!factor_var_2, COUNT) %>%
            tidyr::spread(!!factor_var_2, COUNT) %>%
            dplyr::left_join(t0_1, by = factor_var_1_name) %>%
            dplyr::select(-PERCENT) -> t1_1 -> output3
          
          t1 %>%
            dplyr::select(!!factor_var_1, !!factor_var_2, PERCENT) %>%
            tidyr::spread(!!factor_var_2, PERCENT) %>%
            dplyr::left_join(t0_1, by = factor_var_1_name) %>%
            dplyr::select(-COUNT) -> t1_2 -> output4
          
          t1 %>%
            dplyr::select(!!factor_var_2, !!factor_var_1, COUNT) %>%
            tidyr::spread(!!factor_var_1, COUNT) %>%
            dplyr::left_join(t0_2, by = factor_var_2_name) %>%
            dplyr::select(-PERCENT) -> t1_3 -> output5
          
          t1 %>%
            dplyr::select(!!factor_var_2, !!factor_var_1, PERCENT) %>%
            tidyr::spread(!!factor_var_1, PERCENT) %>%
            dplyr::left_join(t0_2, by = factor_var_2_name) %>%
            dplyr::select(-COUNT) -> t1_4 -> output6
          
          data %>%
            dplyr::select(!!factor_var_1) %>%
            dplyr::group_by(!!factor_var_1) %>%
            dplyr::mutate(count_2 = n()) %>%
            dplyr::distinct() -> t2
          
          data %>%
            dplyr::select(!!factor_var_1, !!factor_var_2) %>%
            dplyr::group_by(!!factor_var_1, !!factor_var_2) %>%
            dplyr::mutate(count_1 = n()) %>%
            dplyr::distinct() %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var_1, !!factor_var_2, fill = list(count_1 = 0)) %>%
            dplyr::mutate(count_1 = ifelse(is.na(count_1), 0, count_1)) %>%
            dplyr::left_join(t2, by = factor_var_1_name) %>%
            dplyr::mutate(percent = round(100 * count_1/count_2, 2)) %>%
            dplyr::select(!!factor_var_1, !!factor_var_2, percent) %>%
            tidyr::spread(!!factor_var_1, percent) -> t1_5 -> output7
          
          data %>%
            dplyr::select(!!factor_var_2) %>%
            dplyr::group_by(!!factor_var_2) %>%
            dplyr::mutate(count_2 = n()) %>%
            dplyr::distinct() -> t2
          
          data %>%
            dplyr::select(!!factor_var_2, !!factor_var_1) %>%
            dplyr::group_by(!!factor_var_2, !!factor_var_1) %>%
            dplyr::mutate(count_1 = n()) %>%
            dplyr::distinct() %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var_2, !!factor_var_1, fill = list(count_1 = 0)) %>%
            dplyr::mutate(count_1 = ifelse(is.na(count_1), 0, count_1)) %>%
            dplyr::left_join(t2, by = factor_var_2_name) %>%
            dplyr::mutate(percent = round(100 * count_1/count_2, 2)) %>%
            dplyr::select(!!factor_var_2, !!factor_var_1, percent) %>%
            tidyr::spread(!!factor_var_2, percent) -> t1_6 -> output8
          
          output <- list(output1, output2, output3, output4, output5, output6, output7, output8)
          
          tabele <- list("STATS_1" = output1,
                         "STATS_2" = output2,
                         "STATS_3" = output3,
                         "STATS_4" = output4,
                         "STATS_5" = output5,
                         "STATS_6" = output6,
                         "STATS_7" = output7,
                         "STATS_8" = output8)
          
          if (stats_save == TRUE){
            if (is.null(save_filename)){
              openxlsx::write.xlsx(tabele, paste0(paste(Sys.Date(), factor_var_1_name, factor_var_2_name), ".xlsx"),
                                   creator = "Adam Nowacki adam.nowacki95@gmail.com",
                                   borderColour = c("black"))
              file_name = paste0(paste(Sys.Date(), factor_var_1_name, factor_var_2_name), ".xlsx")
              print(paste(getwd(), file_name, sep = "/"))
            } else {
              openxlsx::write.xlsx(tabele, paste0(save_filename, ".xlsx"),
                                   creator = "Adam Nowacki adam.nowacki95@gmail.com",
                                   borderColour = c("black"))
              file_name <- paste0(save_filename, ".xlsx")
              print(paste(getwd(), file_name, sep = "/"))
            }
          }
          knitr::kable(output)
        }
      } else {
        print("ERROR: Type of provided factor_var_2 is not appropariate (require factor)")}
    } else {
      print("ERROR: Type of provided factor_var_1 is not appropariate (require factor)")}
  } else {
    print("ERROR: Type of provided data is not appropariate (require tibble or dataframe)")}
}

# plots_factor_factor(data = diamonds,
#                     data_size = 1,
#                     seed_value = 1,
#                     factor_var_1 = cut, factor_axis_1 = "cut",
#                     factor_var_2 = clarity, factor_axis_2 = "clarity",
#                     caption = "diamonds",
#                     plots_print = TRUE, plots_save = FALSE,
#                     stats_print = TRUE, stats_save = FALSE)

# Deep visual analysis factor vs numeric:
plots_factor_numeric <- function(data, factor_var, numeric_var,  
                                 
                                 # Sample data from original datatset to make faster function compilation:
                                 data_size = 0.5,
                                 seed_value = 42,
                                 
                                 # Axis, labels:
                                 factor_axis = NULL, numeric_axis = NULL,  count_axis = "COUNT", percentage_axis = "PERCENTAGE",
                                 density_axis = "DENSITY", caption = NULL,
                                 
                                 # Titles:
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
                                 
                                 # Print:
                                 plots_print = TRUE, stats_print = TRUE,
                                 
                                 # Stats save:
                                 stats_save = FALSE, 
                                 
                                 # Plot save:
                                 plots_save = FALSE, save_filename = NULL, save_width = 40, save_height = 40, save_dpi = 1000,
                                 
                                 # Plot matrix:
                                 plot_grid = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), byrow = TRUE, ncol = 4)
                                 
){
  # Packages:
  if (!require(ggplot2)){install.packages('ggplot2'); require('ggplot2')}
  if (!require(tidyverse)){install.packages('tidyverse'); require('tidyverse')}
  if (!require(dplyr)){install.packages('dplyr'); require('dplyr')}
  if (!require(magrittr)){install.packages('magrittr'); require('magrittr')} 
  if (!require(gridExtra)){install.packages('gridExtra'); require('gridExtra')}
  if (!require(scales)){install.packages('scales'); require('scales')}
  if (!require(stringr)){install.packages('stringr'); require('stringr')}
  if (!require(ggridges)){install.packages('ggridges'); require('ggridges')}
  if (!require(e1071)){install.packages('e1071'); require('e1071')}  
  if (!require(knitr)){install.packages('knitr'); require('knitr')}  
  if (!require(packcircles)){install.packages('packcircles'); require('packcircles')}  
  if (!require(viridis)){install.packages('viridis'); require('viridis')}  
  if (!require(openxlsx)){install.packages('openxlsx'); require('openxlsx')}    
  
  # Additional options:
  options(scipen = 20)
  
  # Variables:
  numeric_var <- enquo(numeric_var)                    
  factor_var  <- enquo(factor_var) 
  numeric_var_name <- quo_name(numeric_var)
  factor_var_name <- quo_name(factor_var)
  
  # Conditions:
  if (is.data.frame(data) | is_tibble(data)){
    print("INFO: Type of provided data is appropriate")
    if (is.factor(data[[which(names(data) == factor_var_name)]])){
      print("INFO: Type of provided factor_var is appropariate")
      if (is.numeric(data[[which(names(data) == numeric_var_name)]]) | is.integer(data[[which(names(data) == numeric_var_name)]])){
        print("INFO: Type of provided numeric_var is appropariate")
        if (is.null(factor_axis)){factor_axis <- "FACTOR VARIABLE"} else {factor_axis <- stringr::str_to_upper(factor_axis)}
        if (is.null(numeric_axis)){numeric_axis <- "NUMERIC VARIABLE"} else {numeric_axis <- stringr::str_to_upper(numeric_axis)}
        if (is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- dplyr::as_tibble(data)
        set.seed(seed = seed_value)
        data %>%
          dplyr::sample_frac(size = data_size) -> data
        
        if(plots_print == TRUE){
          
          # PLOT 1:
          data %>% 
            dplyr::select(!!factor_var) %>% 
            dplyr::group_by(!!factor_var) %>% 
            dplyr::summarise(count = n(), 
                             percentage = n()/nrow(.)) -> data_cut
          data %>% 
            dplyr::select(!!factor_var) -> var; var <- c(var); var <- unlist(var)
          mean_value <- length(var)/length(unique(data[[which(names(data) == factor_var_name)]]))
          
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var, y = count, label = count, fill = !!factor_var)) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 1, col = "black") +
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
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) +
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) +
            ggplot2::scale_y_continuous(limits = c(0, 1.1 * max(data_cut$count))) -> plot1
          
          # PLOT 2:  
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var)
          mean_value <- (length(var)/length(unique(data[[which(names(data) == factor_var_name)]])))/length(var)
          
          ggplot2::ggplot(data = data_cut, mapping = aes(x = !!factor_var, y = percentage, fill = !!factor_var, label = paste(100 * round(percentage, 4), "%", sep = ""))) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 2, lwd = 1, col = "black") +
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
            # ggplot2::scale_fill_hue(scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) 
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
            # ggplot2::scale_fill_manual(values = scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) 
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
            ggplot2::geom_hline(yintercept = mean_value, lty = 1, lwd = 1, color = "black") +
            ggplot2::geom_hline(yintercept = median_value, lty = 2, lwd = 1, color = "black") +    
            ggplot2::geom_hline(yintercept = Q1_value, lty = 2, lwd = 1, color = "black") +   
            ggplot2::geom_hline(yintercept = Q3_value, lty = 2, lwd = 1, color = "black") +   
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
            # ggplot2::scale_fill_manual(values = scales::hue_pal()(length(unique(data[[which(names(data) == factor_var_name)]])))) 
            ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(length(unique(data[[which(names(data) == factor_var_name)]])) + 1, "Greys")[-1]) -> plot5
          
          # PLOT 6:  
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); mean_value <- mean(var, na.rm = TRUE)
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); median_value <- median(var, na.rm = TRUE)
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); Q1_value <- quantile(var, 0.25, na.rm = TRUE)
          data %>% select(!!numeric_var) -> var; var <- c(var); var <- unlist(var); Q3_value <- quantile(var, 0.75, na.rm = TRUE)
          
          ggplot2::ggplot(data = data, aes(x = !!factor_var, y = !!numeric_var, fill = !!factor_var)) +
            ggplot2::geom_hline(yintercept = mean_value, lty = 1, lwd = 1, color = "black") +
            ggplot2::geom_hline(yintercept = median_value, lty = 2, lwd = 1, color = "black") +    
            ggplot2::geom_hline(yintercept = Q1_value, lty = 2, lwd = 1, color = "black") +   
            ggplot2::geom_hline(yintercept = Q3_value, lty = 2, lwd = 1, color = "black") +   
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
        }
        
        if(stats_print == TRUE){
          
          data %>% dplyr::group_by(!!factor_var) %>% dplyr::summarise(MEAN = round(mean(!!numeric_var, na.rm = TRUE), 3),
                                                                      STANDARD_DEVIATION = round(sd(!!numeric_var, na.rm = TRUE), 3),
                                                                      VARIANCE = round(var(!!numeric_var, na.rm = TRUE), 3),
                                                                      Q1 = round(quantile(!!numeric_var, 0.25, na.rm = TRUE), 3),
                                                                      MEDIAN = round(quantile(!!numeric_var, 0.5, na.rm = TRUE), 3),
                                                                      Q3 = round(quantile(!!numeric_var, 0.75, na.rm = TRUE), 3),
                                                                      IQR = round(Q3 - Q1, 3),
                                                                      MAX = round(max(!!numeric_var, na.rm = TRUE), 3),
                                                                      MIN = round(min(!!numeric_var, na.rm = TRUE), 3),
                                                                      COUNT = n(),
                                                                      PERCENT = round(n()/nrow(data), 3),
                                                                      PERCENT = round(100 * (n()/nrow(.)), 3),
                                                                      KURTOSIS = round(e1071::kurtosis(!!numeric_var, na.rm = TRUE), 3),
                                                                      SKEWNESS = round(e1071::skewness(!!numeric_var, na.rm = TRUE), 3)) -> output1
          
          data %>% dplyr::summarise(ALL = factor("ALL"),
                                    MEAN = round(mean(!!numeric_var, na.rm = TRUE), 3),
                                    STANDARD_DEVIATION = round(sd(!!numeric_var, na.rm = TRUE), 3),
                                    VARIANCE = round(var(!!numeric_var, na.rm = TRUE), 3),
                                    Q1 = round(quantile(!!numeric_var, 0.25, na.rm = TRUE), 3),
                                    MEDIAN = round(quantile(!!numeric_var, 0.5, na.rm = TRUE), 3),
                                    Q3 = round(quantile(!!numeric_var, 0.75, na.rm = TRUE), 3),
                                    IQR = round(Q3 - Q1, 3),
                                    MAX = round(max(!!numeric_var, na.rm = TRUE), 3),
                                    MIN = round(min(!!numeric_var, na.rm = TRUE), 3),
                                    COUNT = n(),
                                    PERCENT = round(100 * (n()/nrow(.)), 3),
                                    KURTOSIS = round(e1071::kurtosis(!!numeric_var, na.rm = TRUE), 3),
                                    SKEWNESS = round(e1071::skewness(!!numeric_var, na.rm = TRUE), 3)) -> output2
          names(output2)[1] <- names(data)[[which(names(data) == factor_var_name)]]
          
          data %>%
            dplyr::group_by(!!factor_var) %>%
            dplyr::summarise(COUNT = n()) %>%
            dplyr::ungroup() -> a0
          data %>%
            dplyr::mutate(cuts = ggplot2::cut_number(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::select(!!factor_var, cuts) %>%
            dplyr::group_by(!!factor_var, cuts) %>%
            dplyr::summarise(count = n()) %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var, cuts) %>%
            dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
            tidyr::spread(key = cuts, value = count) %>%
            dplyr::left_join(a0, by = factor_var_name) -> a1
          colnames(a1)[1] <- "factor"
          data %>%
            dplyr::mutate(cuts = ggplot2::cut_number(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::select(cuts) %>%
            dplyr::group_by(cuts) %>%
            dplyr::summarise(count = n()) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts) %>%
            dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
            tidyr::spread(key = cuts, value = count) -> a2
          a2_ <- tibble(factor = "COUNT")
          a2 <- dplyr::bind_cols(a2_, a2)
          a2_ <- tibble(COUNT = nrow(data))
          a2 <- dplyr::bind_cols(a2, a2_)
          a1$factor <- as.character(a1$factor)
          a2 <- dplyr::bind_rows(a1, a2)
          colnames(a2)[1] <- factor_var_name
          output3 <- a2
          
          data %>%
            dplyr::group_by(!!factor_var) %>%
            dplyr::summarise(PERCENT = n()/nrow(data)) %>%
            dplyr::ungroup() -> a0
          data %>%
            dplyr::mutate(cuts = ggplot2::cut_number(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::select(!!factor_var, cuts) %>%
            dplyr::group_by(!!factor_var, cuts) %>%
            dplyr::summarise(percent = n()/nrow(data)) %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var, cuts) %>%
            dplyr::mutate(percent = ifelse(is.na(percent), 0, percent)) %>%
            tidyr::spread(key = cuts, value = percent) %>%
            dplyr::left_join(a0, by = factor_var_name) -> a1
          colnames(a1)[1] <- "factor"
          data %>%
            dplyr::mutate(cuts = ggplot2::cut_number(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::select(cuts) %>%
            dplyr::group_by(cuts) %>%
            dplyr::summarise(percent = n()/nrow(data)) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts) %>%
            dplyr::mutate(percent = ifelse(is.na(percent), 0, percent)) %>%
            tidyr::spread(key = cuts, value = percent) -> a2
          a2_ <- tibble(factor = "PERCENT")
          a2 <- dplyr::bind_cols(a2_, a2)
          a2_ <- tibble(PERCENT = 100)
          a2 <- dplyr::bind_cols(a2, a2_)
          a1$factor <- as.character(a1$factor)
          a2 <- dplyr::bind_rows(a1, a2)
          colnames(a2)[1] <- factor_var_name
          output4 <- a2     
          
          data %>%
            dplyr::mutate(cuts = cut_interval(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::select(!!factor_var, cuts) %>%
            dplyr::group_by(!!factor_var, cuts) %>%
            dplyr::summarise(count = n()) %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var, cuts) %>%
            dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
            tidyr::spread(key = cuts, value = count) -> a1
          colnames(a1)[1] <- "factor"
          data %>%
            dplyr::mutate(cuts = cut_interval(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::select(cuts) %>%
            dplyr::group_by(cuts) %>%
            dplyr::summarise(count = n()) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts) %>%
            dplyr::mutate(count = ifelse(is.na(count), 0, count)) %>%
            tidyr::spread(key = cuts, value = count) -> a2
          a2_ <- tibble(factor = "COUNT")
          a2 <- dplyr::bind_cols(a2_, a2)
          a1$factor <- as.character(a1$factor)
          a2 <- dplyr::bind_rows(a1, a2)
          colnames(a2)[1] <- factor_var_name
          output5 <- a2
          
          data %>%
            dplyr::mutate(cuts = cut_interval(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::select(!!factor_var, cuts) %>%
            dplyr::group_by(!!factor_var, cuts) %>%
            dplyr::summarise(percent = round(n()/nrow(data), 6)) %>%
            dplyr::ungroup() %>%
            tidyr::complete(!!factor_var, cuts) %>%
            dplyr::mutate(percent = ifelse(is.na(percent), 0, percent)) %>%
            tidyr::spread(key = cuts, value = percent) -> a1
          colnames(a1)[1] <- "factor"
          data %>%
            dplyr::mutate(cuts = cut_interval(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::select(cuts) %>%
            dplyr::group_by(cuts) %>%
            dplyr::summarise(percent = round(n()/nrow(data), 6)) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts) %>%
            dplyr::mutate(percent = ifelse(is.na(percent), 0, percent)) %>%
            tidyr::spread(key = cuts, value = percent) -> a2
          a2_ <- tibble(factor = "PERCENT")
          a2 <- dplyr::bind_cols(a2_, a2)
          a1$factor <- as.character(a1$factor)
          a2 <- dplyr::bind_rows(a1, a2)
          colnames(a2)[1] <- factor_var_name
          output6 <- a2
          
          data %>%
            dplyr::mutate(cuts = cut_number(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::group_by(cuts) %>%
            dplyr::summarise(count_2 = n()) -> t1
          data %>%
            dplyr::mutate(cuts = cut_number(!!numeric_var, n = numeric_cuts)) %>%
            dplyr::select(cuts, !!factor_var) %>%
            dplyr::group_by(cuts, !!factor_var) %>%
            dplyr::summarise(count_1 = n()) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts, !!factor_var, fill = list(count_1 = 0)) %>%
            dplyr::left_join(t1, by = "cuts") %>%
            dplyr::mutate(percent = round(100 * count_1/count_2, 2)) %>%
            dplyr::select(cuts, !!factor_var, percent) %>%
            tidyr::spread(!!factor_var, percent) -> output7
          
          output <- list(output1, output2, output3, output4, output5, output6, output7)
          
          tabele <- list("STATS_1" = output1,
                         "STATS_2" = output2,
                         "STATS_3" = output3,
                         "STATS_4" = output4,
                         "STATS_5" = output5,
                         "STATS_6" = output6,
                         "STATS_7" = output7)
          
          if (stats_save == TRUE){
            if (is.null(save_filename)){
              openxlsx::write.xlsx(tabele, paste0(paste(Sys.Date(), factor_var_name, numeric_var_name), ".xlsx"),
                                   creator = "Adam Nowacki adam.nowacki95@gmail.com", 
                                   borderColour = c("black"))
              file_name = paste0(paste(Sys.Date(), factor_var_name, numeric_var_name), ".xlsx")
              print(paste(getwd(), file_name, sep = "/"))
            } else {
              openxlsx::write.xlsx(tabele, paste0(save_filename, ".xlsx"),
                                   creator = "Adam Nowacki adam.nowacki95@gmail.com", 
                                   borderColour = c("black"))
              file_name <- paste0(save_filename, ".xlsx")
              print(paste(getwd(), file_name, sep = "/"))
            }
          }
          knitr::kable(output)
        }
      } else {
        print("ERROR: Type of provided numeric_var is not appropariate (require numeric or integer)")}
    } else {
      print("ERROR: Type of provided factor_var is not appropariate (require factor)")}
  } else {
    print("ERROR: Type of provided data is not appropariate (require tibble or dataframe)")}
}

# plots_factor_numeric(data = diamonds,
#                      data_size = 1,
#                      factor_var = cut, numeric_var = carat,
#                      factor_axis = "cut", numeric_axis = "carat",
#                      caption = "Diamonds",
#                      plots_print = TRUE, plots_save = FALSE,
#                      stats_print = TRUE, stats_save = FALSE)

# Deep visual analysis numeric vs numeric:
plots_numeric_numeric <- function(data, numeric_var_1, numeric_var_2,  
                                  
                                  # Sample data from original datatset to make faster function compilation:
                                  data_size = 0.5,
                                  seed_value = 42,
                                  
                                  # Axis, labels:
                                  numeric_axis_1 = NULL, numeric_axis_2 = NULL, count_axis = "COUNT", percentage_axis = "PERCENTAGE",
                                  density_axis = "DENSITY", caption = NULL,
                                  
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
                                  text_size = 7, title_size = 9, alpha = 0.25, histogram_bins_1 = 25, histogram_bins_2 = 25, hex_bins = 5,
                                  cuts_1 = 5, cuts_2 = 5,
                                  
                                  # Print:
                                  plots_print = TRUE, stats_print = TRUE,
                                  
                                  # Stats save:
                                  stats_save = FALSE, 
                                  
                                  # Plot save:
                                  plots_save = FALSE, save_filename = NULL, save_width = 40, save_height = 40, save_dpi = 1000,
                                  
                                  # Plot matrix:
                                  plot_grid = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), byrow = TRUE, ncol = 4)
                                  
){
  # Packages:
  if (!require(ggplot2)){install.packages('ggplot2'); require('ggplot2')}
  if (!require(tidyverse)){install.packages('tidyverse'); require('tidyverse')}
  if (!require(dplyr)){install.packages('dplyr'); require('dplyr')}
  if (!require(magrittr)){install.packages('magrittr'); require('magrittr')} 
  if (!require(gridExtra)){install.packages('gridExtra'); require('gridExtra')}
  if (!require(scales)){install.packages('scales'); require('scales')}
  if (!require(stringr)){install.packages('stringr'); require('stringr')}
  if (!require(ggridges)){install.packages('ggridges'); require('ggridges')}
  if (!require(e1071)){install.packages('e1071'); require('e1071')}  
  if (!require(knitr)){install.packages('knitr'); require('knitr')}  
  if (!require(packcircles)){install.packages('packcircles'); require('packcircles')}  
  if (!require(viridis)){install.packages('viridis'); require('viridis')}  
  if (!require(openxlsx)){install.packages('openxlsx'); require('openxlsx')}    
  if (!require(hexbin)){install.packages('hexbin'); require('hexbin')} 
  
  # Additional options:
  options(scipen = 20)
  
  # Variables:
  numeric_var_1 <- enquo(numeric_var_1)                    
  numeric_var_2  <- enquo(numeric_var_2) 
  numeric_var_1_name <- quo_name(numeric_var_1)
  numeric_var_2_name <- quo_name(numeric_var_2)
  
  # Conditions:
  if (is.data.frame(data) | is_tibble(data)){
    print("INFO: Type of provided data is appropriate")
    if (is.numeric(data[[which(names(data) == numeric_var_1_name)]]) | is.integer(data[[which(names(data) == numeric_var_1_name)]])){
      print("INFO: Type of provided numeric_var_1 is appropariate")
      if (is.numeric(data[[which(names(data) == numeric_var_2_name)]]) | is.integer(data[[which(names(data) == numeric_var_2_name)]])){
        print("INFO: Type of provided numeric_var_2 is appropariate")
        if (is.null(numeric_axis_1)){numeric_axis_1 <- "NUMERIC VARIABLE 2"} else {numeric_axis_1 <- stringr::str_to_upper(numeric_axis_1)}
        if (is.null(numeric_axis_2)){numeric_axis_2 <- "NUMERIC VARIABLE 2"} else {numeric_axis_2 <- stringr::str_to_upper(numeric_axis_2)}
        if (is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- dplyr::as_tibble(data)
        set.seed(seed = seed_value)
        data %>%
          dplyr::sample_frac(size = data_size) -> data
        
        if(plots_print == TRUE){
          
          # PLOT 1:  
          ggplot2::ggplot(data = data) +
            ggplot2::geom_boxplot(aes(x = 1, y = !!numeric_var_1), fill = "gray60", color = "black", outlier.color = "black", notch = FALSE) + 
            ggplot2::geom_violin(aes(x = 3, y = !!numeric_var_1), draw_quantiles = c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", fill = "gray60", trim = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_1), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_1), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_1), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_1), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::labs(y = numeric_axis_1,
                          title = title_1) +
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
          
          # PLOT 2:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var_1)) +
            ggplot2::geom_histogram(color = "black", bins = histogram_bins_1, fill = "gray60") +  
            ggplot2::geom_freqpoly(bins = histogram_bins_1, lwd = 1, color = "black") +
            ggplot2::labs(x = numeric_axis_1,
                          y = count_axis,
                          title = title_2) +
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
          
          # PLOT 3:
          ggplot2::ggplot(data = data, aes(sample = !!numeric_var_1)) +
            ggplot2::stat_qq() +
            ggplot2::stat_qq_line(lwd = 1, color = "black") +
            ggplot2::labs(x = "THEORETICAL",
                          y = "SAMPLE",
                          title = title_3) +
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
          
          # PLOT 4:
          ggplot2::ggplot(data = data, aes(x = !!numeric_var_1)) +
            ggplot2::stat_density(fill = "gray60", color = "black", lwd = 0.5) +
            ggplot2::stat_ecdf(lwd = 1, color = "black") +
            ggplot2::labs(x = numeric_axis_1,
                          y = percentage_axis,
                          title = title_4) +
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
                                        breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) -> plot4
          
          # PLOT 5:
          ggplot2::ggplot(data = data, mapping = aes(x = !!numeric_var_1, y = !!numeric_var_2)) +
            ggplot2::geom_point(color = "black", alpha = alpha) +
            ggplot2::labs(x = numeric_axis_1,
                          y = numeric_axis_2,
                          title = title_5) +
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
          
          # PLOT 6:
          data %>% 
            select(numeric_var_1_name, numeric_var_2_name) %>% 
            mutate_if(is.numeric, function(x) (x-min(x))/(max(x)-min(x))) -> scaled_data
          
          ggplot(data = scaled_data, aes(x = !!numeric_var_1, y = !!numeric_var_2)) +
            ggplot2::geom_point(color = "black", alpha = alpha) +
            ggplot2::labs(x = numeric_axis_1,
                          y = numeric_axis_2,
                          title = title_6) +
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
                                        breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) +
            ggplot2::scale_x_continuous(labels = scales::percent, limits = c(0, 1),
                                        breaks = c(0.00, 0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00)) -> plot6
          
          # PLOT 7:
          ggplot2::ggplot(data = data, mapping = aes(x = !!numeric_var_1, y = !!numeric_var_2)) +
            ggplot2::geom_hex(bins = hex_bins, colour = "black") +
            ggplot2::labs(x = numeric_axis_1,
                          y = numeric_axis_2,
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
                           legend.box.background = element_rect(color = "black", size = 0.5, linetype = "solid"),
                           legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid", color = "black"),
                           legend.position = "right",
                           legend.box.spacing = unit(0.25, "cm"),
                           legend.text = element_text(size = text_size, color = "black", face = "plain"),
                           legend.title = element_text(size = text_size, color = "black", face = "bold"),
                           legend.key = element_rect(color = "gray90", fill = "gray90", size = 0.5)) +
            ggplot2::scale_fill_gradientn(colours = c("white", "grey0")) +
            ggplot2::guides(fill = guide_legend("COUNT:")) -> plot7
          
          # PLOT 8:
          var_1 <- data[,which(names(data) == numeric_var_1_name)]
          var_2 <- data[,which(names(data) == numeric_var_2_name)]
          t1 <- tibble(korelacja = cor(var_1, var_2), V1 = factor(1), V2 = factor(1))
          colnames(t1)[2:3] <- c(numeric_var_1_name, numeric_var_2_name) 
          ggplot2::ggplot(data = t1, mapping = aes(x = numeric_var_1_name, y = numeric_var_2_name, label = round(korelacja, 2))) +
            geom_tile() +
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
            # ggplot2::scale_fill_manual(breaks = c(-1, 0, 1), values = c("grey0", "grey90", "grey0")) -> plot8
            ggplot2::scale_fill_grey(start = -1, end = 1) -> plot8
          
          # PLOT 9:
          data %>%
            dplyr::mutate(cuts_1 = ggplot2::cut_interval(!!numeric_var_1, cuts_1),
                          cuts_2 = ggplot2::cut_interval(!!numeric_var_2, cuts_2)) %>%
            dplyr::group_by(cuts_1, cuts_2) %>%
            dplyr::summarise(count = n()) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts_1, cuts_2, fill = list(count = 0)) %>%
            ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = count, label = count)) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::labs(x = numeric_axis_1,
                          y = numeric_axis_2,
                          title = title_9) +
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
          
          # PLOT 10:
          data %>%
            dplyr::mutate(cuts_1 = ggplot2::cut_interval(!!numeric_var_1, cuts_1),
                          cuts_2 = ggplot2::cut_interval(!!numeric_var_2, cuts_2)) %>%
            dplyr::group_by(cuts_1, cuts_2) %>%
            dplyr::summarise(percent = round(100 * n()/nrow(data), 2)) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts_1, cuts_2, fill = list(percent = 0)) %>%
            ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = percent, label = paste0(percent, "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::labs(x = numeric_axis_1,
                          y = numeric_axis_2,
                          title = title_9) +
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
          
          # PLOT 11:
          data %>%
            dplyr::mutate(cuts_1 = ggplot2::cut_number(!!numeric_var_1, cuts_1),
                          cuts_2 = ggplot2::cut_number(!!numeric_var_2, cuts_2)) %>%
            dplyr::group_by(cuts_1, cuts_2) %>%
            dplyr::summarise(count = n()) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts_1, cuts_2, fill = list(count = 0)) %>%
            ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = count, label = count)) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::labs(x = numeric_axis_1,
                          y = numeric_axis_2,
                          title = title_11) +
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
          
          # PLOT 12:
          data %>%
            dplyr::mutate(cuts_1 = ggplot2::cut_number(!!numeric_var_1, cuts_1),
                          cuts_2 = ggplot2::cut_number(!!numeric_var_2, cuts_2)) %>%
            dplyr::group_by(cuts_1, cuts_2) %>%
            dplyr::summarise(percent = round(100 * n()/nrow(data), 2)) %>%
            dplyr::ungroup() %>%
            tidyr::complete(cuts_1, cuts_2, fill = list(percent = 0)) %>%
            ggplot2::ggplot(mapping = aes(x = cuts_1, y = cuts_2, fill = percent, label = paste0(percent, "%"))) +
            ggplot2::geom_tile(colour = "black") +
            ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
            ggplot2::labs(x = numeric_axis_1,
                          y = numeric_axis_2,
                          title = title_12) +
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
          
          # PLOT 13:  
          ggplot2::ggplot(data = data) +
            ggplot2::geom_boxplot(aes(x = 1, y = !!numeric_var_2), fill = "gray60", color = "black", outlier.color = "black", notch = FALSE) + 
            ggplot2::geom_violin(aes(x = 3, y = !!numeric_var_2), draw_quantiles = c(0.25, 0.50, 0.75), lwd = 0.5, scale = "width", color = "black", fill = "gray60", trim = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_2), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 1, y = !!numeric_var_2), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_2), fun.y = mean, color = "black", geom = "point", shape = 19, size = 2.5, show.legend = FALSE) +
            ggplot2::stat_summary(aes(x = 3, y = !!numeric_var_2), fun.y = mean, color = "white", geom = "point", shape = 19, size = 1, show.legend = FALSE) +
            ggplot2::labs(y = numeric_axis_2,
                          title = title_1) +
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
          ggplot2::ggplot(data = data, aes(x = !!numeric_var_2)) +
            ggplot2::geom_histogram(color = "black", bins = histogram_bins_2, fill = "gray60") +  
            ggplot2::geom_freqpoly(bins = histogram_bins_2, lwd = 1, color = "black") +
            ggplot2::labs(x = numeric_axis_2,
                          y = count_axis,
                          title = title_2) +
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
          ggplot2::ggplot(data = data, aes(sample = !!numeric_var_2)) +
            ggplot2::stat_qq() +
            ggplot2::stat_qq_line(lwd = 1, color = "black") +
            ggplot2::labs(x = "THEORETICAL",
                          y = "SAMPLE",
                          title = title_3) +
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
          ggplot2::ggplot(data = data, aes(x = !!numeric_var_2)) +
            ggplot2::stat_density(fill = "gray60", color = "black", lwd = 0.5) +
            ggplot2::stat_ecdf(lwd = 1, color = "black") +
            ggplot2::labs(x = numeric_axis_2,
                          y = percentage_axis,
                          title = title_4) +
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
                ggsave(filename = paste0(paste(Sys.Date(), numeric_var_1_name, numeric_var_2_name), ".png"),
                       plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
                plot_name <- paste0(paste(Sys.Date(), numeric_var_1_name, numeric_var_2_name), ".png")
                print(paste(getwd(), plot_name, sep = "/"))
              } else {
                ggsave(filename = paste0(save_filename, ".png"),
                       plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
                plot_name <- paste0(save_filename, ".png")
                print(paste(getwd(), plot_name, sep = "/"))
              }
            }
        }
        
      } else {
        print("ERROR: Type of provided numeric_var_2 is not appropariate (require numeric or integer)")}
    } else {
      print("ERROR: Type of provided numeric_var_1 is not appropariate (require numeric or integer)")}
  } else {
    print("ERROR: Type of provided data is not appropariate (require tibble or dataframe)")}
}

# plots_numeric_numeric(data = diamonds,
#                       data_size = 1, 
#                       cuts_1 = 4, cuts_2 = 5,
#                       numeric_var_1 = price, numeric_var_2 = carat,
#                       numeric_axis_1 = "price", numeric_axis_2 = "carat",
#                       plots_print = TRUE, plots_save = FALSE,
#                       stats_print = TRUE, stats_save = FALSE)


plots_date_numeric <- function(data, date_var, numeric_var,  
                               
                               # Sample data from original datatset to make faster function compilation:
                               data_size = 0.5,
                               seed_value = 42,
                               
                               # Axis, labels:
                               date_axis = NULL, numeric_axis = NULL, year_axis = "YEAR", month_axis = "MONTH", day_axis = "WEEK DAY",
                               caption = NULL,
                               
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
                               text_size = 7, title_size = 9, alpha = 0.5, span = 151,
                               cuts_1 = 5, cuts_2 = 5, 
                               
                               # Print:
                               plots_print = TRUE, stats_print = TRUE,
                               
                               # Stats save:
                               stats_save = FALSE, 
                               
                               # Plot save:
                               plots_save = FALSE, save_filename = NULL, save_width = 40, save_height = 40, save_dpi = 1000,
                               
                               # Plot matrix:
                               plot_grid = matrix(c(1, 2, 4, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), byrow = TRUE, ncol = 4)
                               
){
  # Packages:
  if (!require(ggplot2)){install.packages('ggplot2'); require('ggplot2')}
  if (!require(tidyverse)){install.packages('tidyverse'); require('tidyverse')}
  if (!require(dplyr)){install.packages('dplyr'); require('dplyr')}
  if (!require(magrittr)){install.packages('magrittr'); require('magrittr')} 
  if (!require(gridExtra)){install.packages('gridExtra'); require('gridExtra')}
  if (!require(scales)){install.packages('scales'); require('scales')}
  if (!require(stringr)){install.packages('stringr'); require('stringr')}
  if (!require(ggridges)){install.packages('ggridges'); require('ggridges')}
  if (!require(e1071)){install.packages('e1071'); require('e1071')}  
  if (!require(knitr)){install.packages('knitr'); require('knitr')}  
  if (!require(packcircles)){install.packages('packcircles'); require('packcircles')}  
  if (!require(viridis)){install.packages('viridis'); require('viridis')}  
  if (!require(openxlsx)){install.packages('openxlsx'); require('openxlsx')}    
  if (!require(hexbin)){install.packages('hexbin'); require('hexbin')} 
  if (!require(lubridate)){install.packages('lubridate'); require('lubridate')} 
  # if (!require(ggspectra)){install.packages('ggspectra'); require('ggspectra')} 
  if (!require(ggpmisc)){install.packages('ggpmisc'); require('ggpmisc')} 
  
  # Additional options:
  options(scipen = 20)
  
  # Variables:
  date_var <- enquo(date_var)                    
  numeric_var  <- enquo(numeric_var) 
  date_var_name <- quo_name(date_var)
  numeric_var_name <- quo_name(numeric_var)
  
  # Conditions:
  if (is.data.frame(data) | is_tibble(data)){
    print("INFO: Type of provided data is appropriate")
    if (is.Date(data[[which(names(data) == date_var_name)]])){
      print("INFO: Type of provided date_var is appropariate")
      if (is.numeric(data[[which(names(data) == numeric_var_name)]]) | is.integer(data[[which(names(data) == numeric_var_name)]])){
        print("INFO: Type of provided numeric_var is appropariate")
        if (is.null(date_axis)){date_axis <- "DATE VARIABLE"} else {date_axis <- stringr::str_to_upper(date_axis)}
        if (is.null(numeric_axis)){numeric_axis <- "NUMERIC VARIABLE"} else {numeric_axis <- stringr::str_to_upper(numeric_axis)}
        if (is.null(caption)){caption <- "SOURCE: unknown data source"} else {caption <- stringr::str_to_upper(paste("SOURCE:", caption))}
        
        # Convert data to tibble:
        data <- dplyr::as_tibble(data)
        set.seed(seed = seed_value)
        data %>%
          dplyr::sample_frac(size = data_size) -> data
        
        if(plots_print == TRUE){
          
          # PLOT 1:
          data %>%
            dplyr::select(!!numeric_var) -> var; var <- c(var); var <- unlist(var)
            
            ggplot2::ggplot(data = data, mapping = aes(x = !!date_var, y = !!numeric_var)) +
              ggplot2::geom_point(alpha = alpha) +
              ggplot2::geom_hline(yintercept = mean(var, na.rm = TRUE), lty = 1, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = quantile(var, probs = 0.25, na.rm = TRUE), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = quantile(var, probs = 0.50, na.rm = TRUE), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = quantile(var, probs = 0.75, na.rm = TRUE), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::labs(x = date_axis,
                            y = numeric_axis,
                            title = title_1) +
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
            # PLOT 2:
            ggplot2::ggplot(data = data, mapping = aes(x = !!date_var, y = !!numeric_var)) +
              ggplot2::geom_point(alpha = alpha) +
              ggplot2::geom_smooth(color = "white", method = "lm", se = FALSE,  lty = 1, lwd = 1.5) +
              ggplot2::geom_smooth(color = "white", method = "loess", se = FALSE, lty = 1, lwd = 1.5) +
              ggplot2::geom_smooth(color = "black", method = "lm", se = FALSE,  lty = 1, lwd = 1) +
              ggplot2::geom_smooth(color = "black", method = "loess", se = FALSE, lty = 1, lwd = 1) +
              ggplot2::labs(x = date_axis,
                            y = numeric_axis,
                            title = title_2) +
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
            # PLOT 4:
            data %>%
              ggplot2::ggplot(data = ., mapping = aes(x = !!date_var, y = !!numeric_var, label = paste(round(!!numeric_var, 2), ":", !!date_var))) +
              geom_line(color = "black") + 
              ggpmisc::stat_peaks(colour = "black", span = span) +
              ggpmisc::stat_peaks(geom = "label_repel", span = span, x.label.fmt = "%Y-%m-%d", colour = "black", size = 3, label.size = 0.5,
                                  fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggpmisc::stat_valleys(colour = "black", span = span) +
              ggpmisc::stat_valleys(geom = "label_repel", span = span, x.label.fmt = "%Y-%m-%d", colour = "black", size = 3, label.size = 0.5,
                                    fontface = 1, fill = "white", label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::labs(x = date_axis,
                            y = numeric_axis,
                            title = title_4) +
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
            # PLOT 5:
            data %>%
              mutate(mean_value = mean(!!numeric_var, na.rm = TRUE),
                     year = factor(lubridate::year(!!date_var))) %>%
              group_by(year) %>%
              mutate(mean_group = mean(!!numeric_var, na.rm = TRUE)) %>%
              select(year, mean_value, mean_group) %>%
              distinct() %>%
              mutate(type = ifelse(mean_group < mean_value, "Mean below average", "Mean above average")) %>%
              arrange(mean_group) %>%
              mutate(mean_group = mean_group - mean_value) %>%
              ungroup() -> data_chart
            
            ggplot(data = data_chart, aes(x = year, y = mean_group, label = round(mean_group, 2))) +
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -sd(data_chart$mean_group), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(aes(fill = type), stat = 'identity', color = "black") +
              ggplot2::scale_fill_manual(name = "GROUP:",
                                         labels = c(paste0("MEAN", "\n", "ABOVE", "\n", "AVERAGE", "\n", "MEAN"),
                                                    paste0("MEAN", "\n", "BELOW", "\n", "AVERAGE", "\n", "MEAN")),
                                         values = c("Mean above average" = "grey30",
                                                    "Mean below average" = "grey50")) +
              ggplot2::labs(x = year_axis,
                            y = "DIFFERENCE",
                            title = title_5) +
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
                             legend.title = element_text(size = text_size, color = "black", face = "bold"),
                             legend.key = element_rect(size = 4, fill = "gray90", colour = "gray90"), 
                             legend.key.size = unit(0.75, "cm")) +
              ggplot2::guides(ncol = 1)  -> plot5
            
            # PLOT 6:
            data %>%
              mutate(median_value = median(!!numeric_var, na.rm = TRUE),
                     year = factor(lubridate::year(!!date_var))) %>%
              group_by(year) %>%
              mutate(median_group = median(!!numeric_var, na.rm = TRUE)) %>%
              select(year, median_value, median_group) %>%
              distinct() %>%
              mutate(type = ifelse(median_group < median_value, "Median below average", "Median above average")) %>%
              arrange(median_group) %>%
              mutate(median_group = median_group - median_value) %>%
              ungroup() -> data_chart
            
            ggplot(data = data_chart, aes(x = year, y = median_group, label = round(median_group, 2))) +
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
              ggplot2::labs(x = year_axis,
                            y = "DIFFERENCE",
                            title = title_6) +
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
              ggplot2::guides(ncol = 1) -> plot6
            
            # PLOT 7:
            data %>%
              dplyr::mutate(year = factor(lubridate::year(!!date_var)),
                            month = factor(lubridate::month(!!date_var), levels = 1:12, labels = labels_month)) %>%
              dplyr::group_by(year, month) %>%
              dplyr::summarise(mean = mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() -> tab
            
            tab %>%
              ggplot2::ggplot(data = ., mapping = aes(x = year, y = month, fill = mean, label = round(mean, 2))) +
              ggplot2::geom_tile(colour = "black") +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::labs(x = year_axis,
                            y = month_axis,
                            title = title_7,
                            fill = "MEAN VALUE:") +
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
              ggplot2::guides(ncol = 1) -> plot7
            
            # PLOT 8:
            data %>%
              dplyr::select(!!date_var, !!numeric_var) %>%
              dplyr::mutate(year = lubridate::year(!!date_var)) %>%
              dplyr::group_by(year) %>%
              dplyr::summarise(mean = mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(date = lubridate::make_date(year, 1, 1)) %>%
              dplyr::mutate(difference = mean - lag(mean, n = 1),
                            lag_date = lag(date, n = 1)) %>%
              dplyr::filter(!is.na(difference)) %>%
              dplyr::mutate(type = ifelse(difference < 0, "decrease", "increase")) -> tab
            
            ggplot2::ggplot(data = tab, mapping = aes(x = date, y = difference,  fill = type)) + 
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(stat = "identity", colour = "black") +
              ggplot2::labs(x = date_axis,
                            y = "DIFFERENCE",
                            title = title_8) +
              ggplot2::scale_fill_manual(name = "GROUP:",
                                         labels = c("Decrease", "Increase"),
                                         values = c("increase" = "grey30",
                                                    "decrease" = "grey50")) +
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
              ggplot2::coord_cartesian(ylim = c(-max(abs(tab$difference)), max(abs(tab$difference)))) -> plot8
            
            # PLOT 9:
            data %>%
              mutate(mean_value = mean(!!numeric_var, na.rm = TRUE),
                     month = factor(lubridate::month(!!date_var), levels = 1:12, labels = labels_month)) %>%
              group_by(month) %>%
              mutate(mean_group = mean(!!numeric_var, na.rm = TRUE)) %>%
              select(month, mean_value, mean_group) %>%
              distinct() %>%
              mutate(type = ifelse(mean_group < mean_value, "Mean below average", "Mean above average")) %>%
              arrange(mean_group) %>%
              mutate(mean_group = mean_group - mean_value) %>%
              ungroup() -> data_chart
            
            ggplot(data = data_chart, aes(x = month, y = mean_group, label = round(mean_group, 2))) +
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
              ggplot2::labs(x = month_axis,
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
              mutate(median_value = median(!!numeric_var, na.rm = TRUE),
                     month = factor(lubridate::month(!!date_var), levels = 1:12, labels = labels_month)) %>%
              group_by(month) %>%
              mutate(median_group = median(!!numeric_var, na.rm = TRUE)) %>%
              select(month, median_value, median_group) %>%
              distinct() %>%
              mutate(type = ifelse(median_group < median_value, "Median below average", "Median above average")) %>%
              arrange(median_group) %>%
              mutate(median_group = median_group - median_value) %>%
              ungroup() -> data_chart
            
            ggplot(data = data_chart, aes(x = month, y = median_group, label = round(median_group, 2))) +
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
              ggplot2::labs(x = month_axis,
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
            data %>%
              dplyr::mutate(year = factor(lubridate::year(!!date_var)),
                            day = factor(lubridate::wday(!!date_var, week_start = 1), levels = 1:7, labels = labels_week_day)) %>%
              dplyr::group_by(year, day) %>%
              dplyr::summarise(mean = mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() -> tab
            
            tab %>%
              ggplot2::ggplot(data = ., mapping = aes(x = day, y = year, fill = mean, label = round(mean, 2))) +
              ggplot2::geom_tile(colour = "black") +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::labs(x = day_axis,
                            y = year_axis,
                            title = title_11,
                            fill = "MEAN VALUE:") +
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
              ggplot2::guides(ncol = 1) -> plot11
            
            # PLOT 12:
            data %>%
              dplyr::select(!!date_var, !!numeric_var) %>%
              dplyr::mutate(year = lubridate::year(!!date_var),
                            month = lubridate::month(!!date_var)) %>%
              dplyr::group_by(year, month) %>%
              dplyr::summarise(mean = mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() %>%
              dplyr::mutate(date = lubridate::make_date(year, month, 1)) %>%
              dplyr::mutate(difference = mean - lag(mean, n = 1),
                            lag_date = lag(date, n = 1)) %>%
              dplyr::filter(!is.na(difference)) %>%
              dplyr::mutate(type = ifelse(difference < 0, "decrease", "increase")) -> tab
            
            ggplot2::ggplot(data = tab, mapping = aes(x = date, y = difference,  fill = type)) + 
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(stat = "identity", colour = "black") +
              ggplot2::labs(x = date_axis,
                            y = "DIFFERENCE",
                            title = title_12) +
              ggplot2::scale_fill_manual(name = "GROUP:",
                                         labels = c("Decrease", "Increase"),
                                         values = c("increase" = "grey30",
                                                    "decrease" = "grey50")) +
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
              ggplot2::coord_cartesian(ylim = c(-max(abs(tab$difference)), max(abs(tab$difference)))) -> plot12
            
            # PLOT 13:
            data %>%
              mutate(mean_value = mean(!!numeric_var, na.rm = TRUE),
                     day = factor(lubridate::wday(!!date_var, week_start = 1), levels = 1:7, labels = labels_week_day)) %>%
              group_by(day) %>%
              mutate(mean_group = mean(!!numeric_var, na.rm = TRUE)) %>%
              select(day, mean_value, mean_group) %>%
              distinct() %>%
              mutate(type = ifelse(mean_group < mean_value, "Mean below average", "Mean above average")) %>%
              arrange(mean_group) %>%
              mutate(mean_group = mean_group - mean_value) %>%
              ungroup() -> data_chart
            
            ggplot(data = data_chart, aes(x = day, y = mean_group, label = round(mean_group, 2))) +
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
              ggplot2::labs(x = day_axis,
                            y = "DIFFERENCE",
                            title = title_13) +
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
              ggplot2::guides(ncol = 1) -> plot13
            
            # PLOT 14:
            data %>%
              mutate(median_value = median(!!numeric_var, na.rm = TRUE),
                     day = factor(lubridate::wday(!!date_var, week_start = 1), levels = 1:7, labels = labels_week_day)) %>%
              group_by(day) %>%
              mutate(median_group = median(!!numeric_var, na.rm = TRUE)) %>%
              select(day, median_value, median_group) %>%
              distinct() %>%
              mutate(type = ifelse(median_group < median_value, "Median below average", "Median above average")) %>%
              arrange(median_group) %>%
              mutate(median_group = median_group - median_value) %>%
              ungroup() -> data_chart
            
            ggplot(data = data_chart, aes(x = day, y = median_group, label = round(median_group, 2))) +
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
              ggplot2::labs(x = day_axis,
                            y = "DIFFERENCE",
                            title = title_14) +
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
              ggplot2::guides(ncol = 1) -> plot14
            
            # PLOT 15:
            data %>%
              dplyr::mutate(month = factor(lubridate::month(!!date_var), levels = 1:12, labels = labels_month),
                            day = factor(lubridate::wday(!!date_var, week_start = 1), levels = 1:7, labels = labels_week_day)) %>%
              dplyr::group_by(month, day) %>%
              dplyr::summarise(mean = mean(!!numeric_var, na.rm = TRUE)) %>%
              dplyr::ungroup() -> tab
            
            tab %>%
              ggplot2::ggplot(data = ., mapping = aes(x = day, y = month, fill = mean, label = round(mean, 2))) +
              ggplot2::geom_tile(colour = "black") +
              ggplot2::geom_label(color = "black", size = 3, label.size = 0.5, fontface = 1, fill = "white",label.padding = unit(0.15, "lines"), label.r = unit(0, "lines")) +
              ggplot2::labs(x = day_axis,
                            y = month_axis,
                            title = title_15,
                            fill = "MEAN VALUE:") +
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
            
            # PLOT 16:
            data %>%
              dplyr::select(!!date_var, !!numeric_var) %>%
              dplyr::mutate(difference = !!numeric_var - lag(!!numeric_var, n = 1),
                            lag_date = lag(!!date_var, n = 1)) %>%
              dplyr::filter(!is.na(difference)) %>%
              dplyr::mutate(type = ifelse(difference < 0, "decrease", "increase")) -> tab
            
            ggplot2::ggplot(data = tab, mapping = aes(x = !!date_var, y = difference,  fill = type)) + 
              ggplot2::geom_hline(yintercept = 0, lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 1 * -sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 2 * -sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_hline(yintercept = 3 * -sd(tab$difference), lty = 2, lwd = 0.5, col = "black") +
              ggplot2::geom_bar(stat = "identity") +
              ggplot2::labs(x = date_axis,
                            y = "DIFFERENCE",
                            title = title_16) +
              ggplot2::scale_fill_manual(name = "GROUP:",
                                         labels = c("Decrease", "Increase"),
                                         values = c("increase" = "grey30",
                                                    "decrease" = "grey50")) +
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
              ggplot2::coord_cartesian(ylim = c(-max(abs(tab$difference)), max(abs(tab$difference)))) -> plot16

            dev.new()
            plots <- gridExtra::grid.arrange(arrangeGrob(plot1, plot2, plot4,
                                                         plot5, plot6, plot7, plot8,
                                                         plot9, plot10, plot11, plot12,
                                                         plot13, plot14, plot15, plot16, layout_matrix = plot_grid))
            
            # if (plots_save == TRUE){
            #   if (is.null(save_filename)){
            #     ggsave(filename = paste0(paste(Sys.Date(), numeric_var_1_name, numeric_var_2_name), ".png"),
            #            plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
            #     plot_name <- paste0(paste(Sys.Date(), numeric_var_1_name, numeric_var_2_name), ".png")
            #     print(paste(getwd(), plot_name, sep = "/"))
            #   } else {
            #     ggsave(filename = paste0(save_filename, ".png"),
            #            plot = plots, width = save_width, height = save_height, units = c("cm"), dpi = save_dpi)
            #     plot_name <- paste0(save_filename, ".png")
            #     print(paste(getwd(), plot_name, sep = "/"))
            #   }
            # }
        }
        
      } else {
        print("ERROR: Type of provided numeric_var is not appropariate (require date)")}
    } else {
      print("ERROR: Type of provided date_var is not appropariate (require numeric or integer)")}
  } else {
    print("ERROR: Type of provided data is not appropariate (require tibble or dataframe)")}
}

# Env:
setwd("D:\\Data Scientist\\Visual_kit\\daily-climate-time-series-data"); getwd(); dir()

# Libraries:
library(readr)
library(tidyverse)
library(lubridate)

# Data:
data_1 <- readr::read_csv("DailyDelhiClimateTrain.csv")
data_2 <- readr::read_csv("DailyDelhiClimateTest.csv")
data_1 %>%
  dplyr::bind_rows(data_2) %>%
  dplyr::filter(lubridate::year(date) != 2017) %>%
 # dplyr::filter(lubridate::year(date) %in% c(2014, 2015, 2016)) %>%
  dplyr::arrange(date) -> data; data
data %>%
  dplyr::mutate(date = lubridate::date(date)) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(meantemp = mean(meantemp, na.rm = TRUE)) -> data
plots_date_numeric(data = data,
                   date_var = date, numeric_var = meantemp,
                   date_axis = "Date", numeric_axis = "Temperature")
