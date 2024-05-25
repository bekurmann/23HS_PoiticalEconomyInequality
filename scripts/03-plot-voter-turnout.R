# dependencies
library(tidyverse)
library(here)

here()

# ##############################################################
# turnout
# loading data
turnout <- readRDS("data/raw_turnout_data.rds")

# year as numeric
turnout$Year <- as.numeric(turnout$Year)

plot_turnout <- ggplot(data = turnout) + 
  geom_line(aes(x = Year, y = `Election Turnout %`, group = 1, colour = "Election Turnout %")) +
  geom_line(aes(x = Year, y = `Popular Votes Turnout (Mean, years before) %`, group = 1, colour = "Popular Votes Turnout (Mean)")) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Voter Turnout in % in Switzerland per Year",
       x = "Year",
       y = "Turnout (%)") +
  scale_colour_manual("Legend", 
                      breaks = c("Election Turnout %", "Popular Votes Turnout (Mean)"),
                      values = c("Election Turnout %" = "#AE3A4E", "Popular Votes Turnout (Mean)" = "#435786"),
                      labels = c("Election Turnout %" = "Election Turnout %", "Popular Votes Turnout (Mean)" = "Popular Votes Turnout % \n(Mean, years before)")) +
  scale_x_continuous(breaks = seq(min(turnout$Year), max(turnout$Year), by = 8))

# ##############################################################
# save
ggsave(filename = "img/voter_turnout_per_year.png", plot = plot_turnout, width = 8, height = 6, dpi = 300)
