library(readr)
library(dplyr)
library(lubridate)
library(ggplot2) # for plotting
library(scales) # for plotting

coreCPI <- read_csv("../data/CPILFESL.csv")
df1 <- coreCPI %>%
  mutate(log_coreCPI = log(CPILFESL),
         log_lagcoreCPI = log(lag(CPILFESL)),
         inflationRate = 100 * (log_coreCPI - log_lagcoreCPI)) 

plot <- df1 %>%
  filter(year(ymd(observation_date)) >= 1970) %>%
  ggplot(aes(x = ymd(observation_date), y = inflationRate)) +
  geom_line(color = "black", linewidth = 0.5) +
  labs(
    title = "Core CPI Inflation (1970-Present)",
    x = NULL,
    y = "Inflation Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 15)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)  
  ) +
  scale_x_date(
    date_breaks = "5 years", 
    date_labels = "%Y",
    expand = c(0, 0)
  ) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  coord_cartesian(xlim = c(as.Date("1970-01-01"), NA)) 

# Save with larger dimensions for better resolution
ggsave("../figures/raw_core_cpi_inflation.png", 
       plot = plot,
       width = 10,
       height = 5,
       dpi = 300,
       bg = "white")
