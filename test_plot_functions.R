# ------------------------------------------------------------------------------
# TEST PLOT FUNCTIONS
# ------------------------------------------------------------------------------
base::source("D:/GitHub/VisToolR/plot_factor_factor.R")
base::source("D:/GitHub/VisToolR/plot_factor_numeric.R")
base::library(tidyverse)

base::getwd()

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
                   label_percent_round = 1,
                   variables_as_string = TRUE)

# 2. Diminish dataset to speed up function compilation:
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   variables_as_string = TRUE,
                   data_size = 0.25,
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
                   title_size = 11,
                   text_size = 9,
                   label_size = 5,
                   variables_as_string = TRUE)

# 5. Change grid density in waffle chart and percentage breaks:
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   grid_size = 10,
                   percentage_breaks = 6,
                   variables_as_string = TRUE)

# 6. Save options (default options):
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   # save_plots = TRUE, # uncomment to save
                   variables_as_string = TRUE)

# 7. Save options (custom options):
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   save_plots = TRUE,
                   save_filename = "plot_factor_factor_function_usage_presentation",
                   save_width = 60,
                   save_height = 30,
                   save_dpi = 150,
                   save_file_format = ".jpg",
                   save_plots_units = "cm",
                   variables_as_string = TRUE)
base::unlink("plot_factor_factor_function_usage_presentation.jpg")

# 8. Automatic visualization for all factor variables: target vs other:
target <- "cut"; target
other <- diamonds %>%
  dplyr::select_if(is.factor) %>%
  base::colnames(.) %>%
  .[. != target]; other

create_dir <- "plot_factor_factor_one_vs_all"
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

# 9. Automatic visualization for all factor variables: all vs all:
all <- other <- diamonds %>%
  dplyr::select_if(is.factor) %>%
  base::colnames(.) %>%
  base::expand.grid(., .) %>%
  dplyr::filter(Var1 != Var2) %>%
  dplyr::transmute_all(as.character) %>%
  tibble::as_tibble(); all

create_dir <- "plot_factor_factor_all_vs_all"
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

# ------------------------------------------------------------------------------
# Function - plot_factor_factor:
# 1. Basic function usage:
plot_factor_numeric(data = diamonds,
                    factor_var = cut,
                    numeric_var = carat)
# or:
plot_factor_numeric(data = diamonds,
                    factor_var = "cut",
                    numeric_var = "carat",
                    variables_as_string = TRUE)

# 2. Diminish dataset to speed up function compilation:
plot_factor_numeric(data = diamonds,
                    factor_var = "cut",
                    numeric_var = "carat",
                    data_size = 0.5,
                    seed_value = 10,
                    variables_as_string = TRUE)

# 3. Set axises names and caption:
plot_factor_numeric(data = diamonds,
                    factor_var = "cut",
                    numeric_var = "carat",
                    factor_axis = "CUT",
                    numeric_axis = "CARAT",
                    caption = "DIAMONDS",
                    variables_as_string = TRUE)

# 4. Change font of titles, labels and axises,  number of decimals in 
# distribution quantile cut plot and percentage breaks:
plot_factor_numeric(data = diamonds,
                    factor_var = "cut",
                    numeric_var = "carat",
                    title_size = 9,
                    text_size = 8,
                    label_size = 4,
                    digits_lab = 5,
                    percentage_breaks = 6,
                    variables_as_string = TRUE)

# 5. Change:
# * grid density in waffle chart,
# * quantile cuts in distribution quantile cut plot,
# * number of bars in histogram:
plot_factor_numeric(data = diamonds,
                    factor_var = "cut",
                    numeric_var = "carat",
                    grid_size = 10,
                    numeric_cuts = 4,
                    histogram_bins = 5,
                    variables_as_string = TRUE)

# 6. Save options (default options):
plot_factor_numeric(data = diamonds,
                    factor_var = "cut",
                    numeric_var = "carat",
                    # save_plots = TRUE, # uncomment to save
                    variables_as_string = TRUE)

# 7. Save options (custom options):
plot_factor_numeric(data = diamonds,
                    factor_var = "cut",
                    numeric_var = "carat",
                    save_plots = TRUE,
                    save_filename = "plot_factor_numeric_function_usage_presentation",
                    save_width = 60,
                    save_height = 30,
                    save_dpi = 150,
                    save_file_format = ".jpg",
                    save_plots_units = "cm",
                    variables_as_string = TRUE)
base::unlink("plot_factor_numeric_function_usage_presentation.jpg")

# 8. Automatic visualization for all factor variables: target vs other:
target <- "carat"; target
other <- diamonds %>%
  dplyr::select_if(is.factor) %>%
  base::colnames(.); other

create_dir <- "plot_factor_numeric_all_vs_one"
base::dir.create(create_dir)
for (i in base::seq_along(other)){
  base::setwd(base::paste(base::getwd(), create_dir, sep = "/"))
  plot_factor_numeric(data = diamonds,
                      factor_var = other[i],
                      numeric_var = target,
                      factor_axis = other[i],
                      numeric_axis = target,
                      save_plots = TRUE,
                      variables_as_string = TRUE)
  base::setwd("..")}

# 9. Automatic visualization for all numeric variables: other vs target:
target <- "cut"; target
other <- diamonds %>%
  dplyr::select_if(is.numeric) %>%
  base::colnames(.); other

create_dir <- "plot_factor_numeric_one_vs_all"
base::dir.create(create_dir)
for (i in base::seq_along(other)){
  base::setwd(base::paste(base::getwd(), create_dir, sep = "/"))
  plot_factor_numeric(data = diamonds,
                      factor_var = target, 
                      numeric_var = other[i],
                      factor_axis = target,
                      numeric_axis = other[i],
                      save_plots = TRUE,
                      variables_as_string = TRUE)
  base::setwd("..")}

# 10. Automatic visualization for all variables: all vs all:
factor_variables <- diamonds %>%
  dplyr::select_if(is.factor) %>%
  base::colnames(.); factor_variables

numeric_variables <- diamonds %>%
  dplyr::select_if(is.numeric) %>%
  base::colnames(.); numeric_variables

all <- base::expand.grid(factor_variables, numeric_variables) %>%
  tibble::as_tibble() %>%
  dplyr::transmute_all(as.character); all

create_dir <- "plot_factor_numeric_all_vs_all"
base::dir.create(create_dir)
for (i in 1:base::nrow(all)){
  base::setwd(base::paste(base::getwd(), create_dir, sep = "/"))
  plot_factor_numeric(data = diamonds,
                      factor_var = all$Var1[i], 
                      numeric_var = all$Var2[i],
                      factor_axis = all$Var1[i],
                      numeric_axis = all$Var2[i],
                      save_plots = TRUE,
                      variables_as_string = TRUE)
  base::setwd("..")}




scales::percent_format()