
## da packages
library(tidyverse)
library(scales)

## read data
wfer <- read_csv("data/wfer.csv", col_types = cols(acres = col_number()))

## add intervals to match ArcGIS quantiles (mostly for labels)
wfer <- wfer %>%
  mutate(quantiles = cut(wfer, 
                        breaks = c(
                          -1,
                          20,
                          40,
                          60,
                          80,
                          100),
                        labels = c(
                          "0 - 20",
                          "20 - 40",
                          "40 - 60",
                          "60 - 80",
                          "80 - 100")
  ))

# Reorder levels of quantiles in reverse order
wfer_quantiles$quantiles <- factor(wfer_quantiles$quantiles, levels = rev(levels(wfer_quantiles$quantiles)))

# group by quantiles for chart
wfer_quantiles <- wfer %>%
  group_by(quantiles) %>%
  summarize(total_acres = sum(acres)) %>%
  mutate(percentage = round(total_acres/sum(total_acres)*100))

# Reorder levels of quantiles in reverse order
wfer_quantiles$quantiles <- factor(wfer_quantiles$quantiles, levels = rev(levels(wfer_quantiles$quantiles)))

# make chart

wfer_quantiles_chart <-
  ggplot(wfer_quantiles, aes(x = quantiles, y = total_acres, fill = quantiles)) +
  geom_bar(stat = 'identity', color = '#3d3d3d') +
  coord_flip() +
  labs(
    x = "",
    y = "Total acres per category",
    title = "Wildfire Exposure Risk",
    subtitle = "Colors match map",
    caption = "Categorized; 0 - 20 is the lowest risk category, 80 -100 the highest.") +
  scale_fill_manual(values = c(
    "#FFFFFF",
    "#F3F583",
    "#A5C48C",
    "#5EA79F",
    "#1F3D5C")) +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = paste0("  ", percentage, "%")),
            vjust = -0.5, 
            hjust = -0.10, 
            color = "#3d3d3d",
            size = 4) + 
  theme_bw(base_size = 18) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = 'none')+
  # theme(plot.margin = margin(0, #top
  #                            3, #right
  #                            0, # bottom
  #                            0, #left
  #                            "cm")) + 
  expand_limits(y = 4000000) 


wfer_quantiles_chart
