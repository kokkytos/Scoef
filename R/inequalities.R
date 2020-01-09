
#Lorenz curve
plot_lorenz_curve <- function(tbl){
  tbl %>% 
    mutate(Year = as.factor(years)) %>%
    ggplot( aes(SoL, colour = Year)) +
    stat_lorenz(desc = F) +
    coord_fixed() +
    geom_abline(linetype = "dashed") +
    theme_minimal() +
    hrbrthemes::scale_x_percent() +
    hrbrthemes::scale_y_percent() +
    hrbrthemes::theme_ipsum_rc() +
    labs(x = "Cumulative Percentage of SoL",
         y = "Cumulative Percentage of Municipalities",
         title = "SoL Inequality Among Municipalities",
         caption = "Lorenz curves") -> g
  return(g)
}


#Gini coefficient
plot_gini_coefficient <- function(tbl){
  df %>%
    dplyr::group_by(years) %>% 
    dplyr::summarise(gini = Gini(SoL)) %>% 
    ggplot( aes(x = years, y = gini)) +
    geom_line() +
    geom_point() +
    ylim(0, 1) +
    theme_minimal() +
    hrbrthemes::theme_ipsum_rc() +
    labs(x = "Years",
         y = "Gini coefficient",
         title = "Gini coefficient for SoL Among Municipalities",
         caption = "Source:") 
}

