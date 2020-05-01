# ------------------------------------------------------------------------------
# TEST PLOT FUNCTIONS
# ------------------------------------------------------------------------------
base::source("D:/GitHub/VisToolR/plot_factor_factor.R")
base::library(tidyverse)

# ------------------------------------------------------------------------------
# Function - plot_factor_factor:
# 1. Basic function usage:
plot_factor_factor(data = diamonds,
                   factor_var_1 = cut,
                   factor_var_2 = clarity)
# or:
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   variables_as_string = TRUE)

# 2. Diminish dataset to speed up function compilation:
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   variables_as_string = TRUE,
                   data_size = 0.5,
                   seed_value = 10)

# 3. Set axises names and caption:
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   factor_axis_1 = "CUT",
                   factor_axis_2 = "CLARITY",
                   caption = "DIAMONDS",
                   variables_as_string = TRUE)

# 4. Change font of titles, labels and axises:
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   title_size = 9,
                   text_size = 8,
                   label_size = 3.5,
                   variables_as_string = TRUE)

# 5. Change grid density in waffle chart:
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   grid_size = 10,
                   variables_as_string = TRUE)

# 6. Save options (default options):
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   save_plots = TRUE,
                   variables_as_string = TRUE)

# 7. Save options (custom options):
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   save_plots = TRUE,
                   save_filename = "plot_factor_factor function usage presentation",
                   save_width = 60,
                   save_height = 30,
                   save_dpi = 150,
                   save_file_format = ".jpg",
                   save_plots_units = "cm",
                   variables_as_string = TRUE)
base::unlink("plot_factor_factor usage presentation.jpg")

# 8. Automatic visualization for all factor variables: target vs other:
target <- "cut"; target
other <- diamonds %>%
  dplyr::select_if(is.factor) %>%
  base::colnames(.) %>%
  .[. != target]; other

create_dir <- "plot_factor_factor 1 vs other"
base::dir.create(create_dir)
for (i in base::seq_along(other)){
  base::setwd(base::paste(base::getwd(), create_dir, sep = "/"))
  plot_factor_factor(data = diamonds,
                     factor_var_1 = target,
                     factor_var_2 = other[i],
                     factor_axis_1 = target,
                     factor_axis_2 = other[i],
                     label_size = 3.5,
                     text_size = 9,
                     variables_as_string = TRUE,
                     save_plots = TRUE)
  base::setwd("..")}
base::getwd()

# 9. Automatic visualization for all factor variables: all vs all:
all <- other <- diamonds %>%
  dplyr::select_if(is.factor) %>%
  base::colnames(.) %>%
  base::expand.grid(., .) %>%
  dplyr::filter(Var1 != Var2) %>%
  dplyr::transmute_all(as.character); all

create_dir <- "plot_factor_factor all vs all"
base::dir.create(create_dir)
for (i in 1:base::nrow(all)){
  base::setwd(base::paste(base::getwd(), create_dir, sep = "/"))
  plot_factor_factor(data = diamonds,
                     factor_var_1 = all$Var1[i],
                     factor_var_2 = all$Var2[i],
                     factor_axis_1 = all$Var1[i],
                     factor_axis_2 = all$Var2[i],
                     label_size = 3.5,
                     text_size = 9,
                     variables_as_string = TRUE,
                     save_plots = TRUE)
  base::setwd("..")}
base::getwd()

i <- 1

# ------------------------------------------------------------------------------
