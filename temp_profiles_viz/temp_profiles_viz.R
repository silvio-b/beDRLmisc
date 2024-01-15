library(ggplot2)
library(tidyverse)

# Importing data and fixing datetime
df <- read.csv("temp_profiles_viz/data/data.csv", sep=';', dec=',') 
df$datetime <- as.POSIXct(df$datetime, format ="%Y-%m-%d %H:%M:%S")


# subsetting the two dataframes to show specific days
interval_start = "2024-12-10"
interval_end = "2024-12-13"

df_plot <- df[(df$datetime >= interval_start) & (df$datetime < interval_end),]

# Creating a support dataframe for the visualization
df_occ_range <- df_plot %>%
  mutate(
    date = as.Date(datetime)
  ) %>%
  group_by(
    date, tint_sp
  ) %>%
  summarise(
    min_datetime = min(datetime),
    max_datetime = max(datetime)
  ) %>%
  filter(tint_sp==21) 

# create arrays to assign colors to specific phenomena
fill_colors = c("Occupancy period"="yellow",
               "Acceptability range"="green4")
col_colors = c("Indoor air"="cornflowerblue",
               "Not acceptable values"="red")

# Define custom breaks for x-axis
x_axis_breaks <- c(c(df_occ_range$min_datetime, df_occ_range$max_datetime),
                   unique(as.Date(df_plot$datetime)))

# Build the plot
g1 <- ggplot()+
  # 1st layer: rectangles identifying occupancy periods
  geom_rect(data=df_occ_range, aes(xmin = min_datetime,
                                        xmax = max_datetime,
                                        ymin = 15,
                                        ymax = 24,
                                        fill = "Occupancy period"),
            alpha = 0.2) +
  # 2nd layer: rectangles identifying acceptability range
  geom_rect(data=df_occ_range, aes(xmin = min_datetime,
                                        xmax = max_datetime,
                                        ymin = 20, 
                                        ymax = 22,
                                        fill = "Acceptability range"),
            alpha = 0.2) +
  # 3rd layer: line plot of indoor air temperaure values
  geom_line(data = df_plot, aes(x = datetime,
                                y = tint,
                                color = "Indoor air"), linewidth = 1) +
  # 4th layer: scatter plot of indoor air temperature values excluding points falling
  # outside the acceptability range
  geom_point(data = df_plot[!(df_plot$tint < 20 & df_plot$tint_sp > 17) | 
                              !(df_plot$tint > 22 & df_plot$tint_sp > 17),], 
             aes(x = datetime,
                 y = tint),
             color='cornflowerblue') +
  # 5th layer: scatter plot of indoor air temperature values including only points falling
  # inside the acceptability range
  geom_point(data = df_plot[(df_plot$tint < 20 & df_plot$tint_sp > 17) | 
                              (df_plot$tint > 22 & df_plot$tint_sp > 17),], 
             aes(x = datetime,
                 y = tint,
                 color='Not acceptable values'), size = 2, shape = 4) +
  # adding scales
  scale_x_datetime(expand = c(0,0), breaks = x_axis_breaks,
                   labels = scales::date_format("%m-%d %H:%M", tz="CET")) +
  scale_y_continuous(expand = c(0,0), breaks = seq(15, 24, 1)) +
  scale_fill_manual(values = fill_colors) +
  scale_color_manual(values = col_colors) +
  # setting theme
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    axis.title = element_text(size=14, face = 'bold'),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(angle = -45, hjust = 0),
    legend.text = element_text(size =14),
    legend.position = 'top'
  ) + 
  # adding labels
  labs(
    x = 'Datetime',
    y = 'Temperature [°C]',
    fill = "",
    color = ""
  )

ggsave("temp_profiles_viz/figures/temperature_profiles.png", g1, width = 1600, height = 900,
       units = "px", dpi = 150)
  