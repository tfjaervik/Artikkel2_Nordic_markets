################################################################################
#' Create figure 10 - number of firms per month for all indices in one figure
################################################################################

##### Load packages -------

library(tidyverse)
library(patchwork)

# End of 'Load packages'---


################################################################################
#' Load data
################################################################################

# We load the monthly stock file
load("data/rdata/cleaned_data_for_analysis/stock_data_monthly.Rdata")


################################################################################
#' Create figure
################################################################################

plot_data <- df %>% 
  filter(date < "2022-01-01") %>% 
  mutate(YearMonth = zoo::as.yearmon(date)) %>% 
  group_by(YearMonth) %>% 
  summarise(n = length(unique(company))) 

fig_1 <- ggplot(data = plot_data, mapping = aes(x = YearMonth, y = n)) +
  geom_point() +
  labs(title = "Number of stocks per date in the sample",
    subtitle = "at least 100 valid return observations per year") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + # center the titleand subtitle
  xlab("date") +
  ylab("number of firms")


# Export figure as a .tex file

TexTools::save_tikz(fig_1, path = "fig_10") 
# 
# %>% 
#   TexTools::ltx_caption("This plot shows the number of firm observations per month
#                         in the Nordic market. Each stock in the sample is required
#                         to have at least 100 return observations per year.") 


