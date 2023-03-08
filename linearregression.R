library(dplyr)
library(ggplot2)

data_dir <- file.path("data","raw")

# Data from
# https://catalog.data.gov/dataset/electric-vehicle-population-data
#data_url <- "https://data.wa.gov/api/views/f6w7-q2d2/rows.csv?accessType=DOWNLOAD"
#data_file <- file.path(data_dir, "Electric_Vehicle_Population_Data.csv")

# https://catalog.data.gov/dataset/crime-data-from-2020-to-present
data_url <- "https://data.lacity.org/api/views/2nrs-mtv8/rows.csv?accessType=DOWNLOAD"
data_file <- file.path(data_dir, "Crime-Data-from-2020-to-Present.csv")

if (!file.exists(data_file)) {
  download.file(data_url, data_file)
}

crime_data <- read.csv(data_file)
selected <- crime_data %>% 
  select(TIME.OCC, LAT, LON) %>% 
  mutate(hour = floor(TIME.OCC/100)) %>% 
  mutate(LAT = as.factor(round(LAT, 2))) %>% 
  mutate(LON = as.factor(round(LON, 2))) %>% 
  select(-TIME.OCC)

agged <- selected %>% 
  group_by(hour, LAT, LON) %>%
  summarise(n = n())

agged %>% ggplot(aes(x = LON, y = LAT)) +
  geom_bin2d() +
  facet_grid()

model <- lm(n ~ hour, data = agged)
model.predict <- cbind(agged, predict(model, interval = 'confidence'))

model.predict %>% 
  mutate(hourfct = as.numeric(hour)) %>% 
  ggplot(aes(hour,n)) +
  geom_violin(aes(group = cut_width(hour, 1)), na.rm = TRUE) +
  geom_line(aes(hour, fit)) +
  geom_point(aes(hour, fit)) +
  geom_ribbon(aes(ymin=lwr,ymax=upr), alpha=0.3) +
  lims(y = c(0,100)) +
  labs(title = "Linear regression of number of crimes by hour")

       