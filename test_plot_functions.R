
plot_factor_factor(data = diamonds,
                   factor_var_1 = "cut",
                   factor_var_2 = "clarity",
                   variables_as_string = TRUE,
                   data_size = 0.5,
                   factor_axis_1 = "cut",
                   factor_axis_2 = "clarity",
                   caption = "Diamonds dataset",
                   save_plots = TRUE)

plot_factor_factor(data = diamonds,
                   factor_var_1 = cut,
                   factor_var_2 = clarity,
                   variables_as_string = FALSE)