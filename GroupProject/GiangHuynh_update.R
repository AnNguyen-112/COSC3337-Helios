library(dplyr)
library(ggplot2)
library(plotly)
library(magick)
library(gapminder)
library(gganimate)
install.packages("gifski")
library(gifski_renderer)

data1 <- read_csv("Dataset/rhessi_full2002_2018_filtered.csv")
data2 <- read_csv("Dataset/Solar_flare_RHESSI_2004_05.csv")
data3 <- read_csv("Dataset/Solar_flare_RHESSI_2015_16.csv")


#data2$dt.start <- as.POSIXct(data2$dt.start, format = "%Y-%m-%d %H:%M:%S")
#data2 <- data2 %>% select(-starts_with("V"))

calculate_intensity_method1 <- function(total_counts) {
  intensity <- as.numeric(total_counts)
  return(intensity)
}

calculate_intensity_method2 <- function(duration, energy_band) {
  duration_weight <- 0.6
  energy_weight <- 0.4
  energy_values <- c(9, 18.5, 37.5, 75, 200, 550, 3900, 13500)
  intensity <- (as.numeric(duration) * duration_weight) + 
    (energy_values[match(energy_band, c('6-12', '12-25', '25-50', '50-100', '100-300', '300-800', '800-7000', '7000-20000'))] * energy_weight)
  return(intensity)
}

batch1_data <- data2 %>% filter(year == 2004 & month >= 1 & month <= 4)

batch11_data <- data2 %>% filter(year == 2005 & month >= 9 & month <= 12)

batch1_data <- batch1_data %>%
  mutate(intensity_method1 = calculate_intensity_method1(total.counts),
         intensity_method2 = calculate_intensity_method2(duration.s, energy.kev))
batch11_data <- batch11_data %>%
  mutate(intensity_method1 = calculate_intensity_method1(total.counts),
         intensity_method2 = calculate_intensity_method2(duration.s, energy.kev))

heatmap_batch1_method1 <- ggplot(batch1_data, aes(x = x.pos.asec, y = y.pos.asec, color = intensity_method1)) +
  geom_point(aes(size = energy.kev.i)) +  # Use initial energy value for size
  scale_size_continuous(name = "Initial Energy (keV)", breaks = c(6, 12, 25, 50, 100, 300, 800, 7000)) +  # Assuming these are the lower bounds of your energy bands
  scale_color_gradient(low = "green", high = "red") +
  labs(title = "Intensity Map: Year 2004, Months 1-4 (Method 1)", x = "X Position", y = "Y Position") +
  theme_minimal()

heatmap_batch1_method2 <- ggplot(batch1_data, aes(x = x.pos.asec, y = y.pos.asec, color = intensity_method2)) +
  geom_point(aes(size = energy.kev.i)) +  # Use initial energy value for size
  scale_size_continuous(name = "Initial Energy (keV)", breaks = c(6, 12, 25, 50, 100, 300, 800, 7000)) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = "Intensity Map: Year 2004, Months 1-4 (Method 2)", x = "X Position", y = "Y Position") +
  theme_minimal()

heatmap_batch11_method1 <- ggplot(batch11_data, aes(x = x.pos.asec, y = y.pos.asec, color = intensity_method1)) +
  geom_point(aes(size = energy.kev.i)) +
  scale_size_continuous(name = "Initial Energy (keV)", breaks = c(6, 12, 25, 50, 100, 300, 800, 7000)) +
  scale_color_gradient(low = "pink", high = "red") +
  labs(title = "Intensity Map: Year 2005, Months 9-12 (Method 1)", x = "X Position", y = "Y Position") +
  theme_minimal()

heatmap_batch11_method2 <- ggplot(batch11_data, aes(x = x.pos.asec, y = y.pos.asec, color = intensity_method2)) +
  geom_point(aes(size = energy.kev.i)) +
  scale_size_continuous(name = "Energy (keV)", breaks = c(6, 12, 25, 50, 100, 300, 800, 7000)) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Intensity Map: Year 2005, Months 9-12 (Method 2)", x = "X Position", y = "Y Position") +
  theme_minimal()

print(heatmap_batch1_method1)
print(heatmap_batch1_method2)
print(heatmap_batch11_method1)
print(heatmap_batch11_method2)


# Task 2
library(dplyr)
library(ggplot2)
library(dbscan)
library(plotly)

#task A
#task B
threshold_d1 <- quantile(batch1_data$intensity_method1, probs = 0.90)
threshold_d2 <- quantile(batch1_data$intensity_method1, probs = 0.75)
#end task B

hot_spots_d1 <- batch1_data %>% filter(intensity_method1 > threshold_d1)
hot_spots_d2 <- batch1_data %>% filter(between(intensity_method1, threshold_d2, threshold_d1))

eps <- 1
minPts <- 5
#end task A

#TASK C
db_d1 <- dbscan(hot_spots_d1[, c("x.pos.asec", "y.pos.asec")], eps = eps, minPts = minPts)
db_d2 <- dbscan(hot_spots_d2[, c("x.pos.asec", "y.pos.asec")], eps = eps, minPts = minPts)

hot_spots_d1$cluster <- db_d1$cluster
hot_spots_d2$cluster <- db_d2$cluster

hot_spots <- rbind(
  hot_spots_d1 %>% mutate(hotspot_type = 'Very Hot Spot'),
  hot_spots_d2 %>% mutate(hotspot_type = 'Regional Hot Spot')
)

p <- ggplot(batch1_data, aes(x = x.pos.asec, y = y.pos.asec)) +
  geom_point(aes(color = as.factor(energy.kev)), alpha = 0.5) + # Base layer with all points
  geom_point(data = hot_spots, aes(size = intensity_method1, shape = hotspot_type), alpha = 0.8) +
  scale_shape_manual(values = c(16, 17)) + # Shapes for different hotspot types
  scale_color_brewer(palette = "Spectral") + # Colors for energy bands
  scale_size(range = c(2, 8)) + # Adjust point sizes
  labs(title = "Intensity and Energy Bands of Solar Flares", x = "X Position", y = "Y Position",
       color = "Energy Band", size = "Intensity (Method 1)", shape = "Hotspot Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

interactive_plot <- ggplotly(p, tooltip = c("x", "y", "color", "size", "shape"))

interactive_plot

#TESTING
p <- ggplot(batch1_data, aes(x = x.pos.asec, y = y.pos.asec)) +
  geom_point(aes(color = as.factor(energy.kev)), alpha = 0.5) + # Base layer with all points, colored by energy band
  geom_point(data = hot_spots, aes(size = intensity_method1, shape = as.factor(hotspot_type), color = as.factor(hotspot_type)), alpha = 0.8) +
  scale_shape_manual(values = c("Very Hot Spot" = 17, "Regional Hot Spot" = 15)) + # Shapes for different hotspot types
  scale_color_manual(values = c("Very Hot Spot" = "pink", "Regional Hot Spot" = "lightsalmon")) + # Colors for hotspot types
  scale_size(range = c(2, 8)) + # Adjust point sizes
  labs(title = "Intensity and Energy Bands of Solar Flares", x = "X Position", y = "Y Position",
       color = "Hotspot Type", size = "Intensity (Method 1)", shape = "Hotspot Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

interactive_plot <- ggplotly(p, tooltip = c("x", "y", "color", "size", "shape"))
interactive_plot

#Part D and E
#CREATING 11 BATCHES
batch1_data <- data2 %>% filter(year == 2004 & month >= 1 & month <= 4)
batch2_data <- data2 %>% filter(year == 2004 & month >= 3 & month <= 6)
batch3_data <- data2 %>% filter(year == 2004 & month >= 5 & month <= 8)
batch4_data <- data2 %>% filter(year == 2004 & month >= 7 & month <= 10)
batch5_data <- data2 %>% filter(year == 2004 & month >= 9 & month <= 12)
batch6_data <- data2 %>% filter(year == 2004 & month >= 11 & month <= 12 | year == 2005 & month >= 1 & month <= 2)
batch7_data <- data2 %>% filter(year == 2005 & month >= 1 & month <= 4)
batch8_data <- data2 %>% filter(year == 2005 & month >= 3 & month <= 6)
batch9_data <- data2 %>% filter(year == 2005 & month >= 5 & month <= 8)
batch10_data <- data2 %>% filter(year == 2005 & month >= 7 & month <= 10)
batch11_data <- data2 %>% filter(year == 2005 & month >= 9 & month <= 12)
#CREATING A LIST THAT CONTAINS 11 BATCHES
batch_data_list <- list(batch1_data, 
                        batch2_data, 
                        batch3_data, 
                        batch4_data,
                        batch5_data, 
                        batch6_data, 
                        batch7_data, 
                        batch8_data,
                        batch9_data,
                        batch10_data,
                        batch11_data) # Replace with actual data frames
hotspot_time_series_d1 <- list()
hotspot_time_series_d2 <- list()
hotspot_time_series <- list()
for(i in seq_along(batch_data_list)) {
  batch_data <- batch_data_list[[i]]
  batch_data <- batch_data %>%
    mutate(intensity_method1 = calculate_intensity_method1(total.counts),
           intensity_method2 = calculate_intensity_method2(duration.s, energy.kev))
  batch_data <- batch_data %>%
    mutate(intensity_method1 = calculate_intensity_method1(total.counts),
           intensity_method2 = calculate_intensity_method2(duration.s, energy.kev))
  # Calculate thresholds for the current batch
  threshold_d1 <- quantile(batch_data$intensity_method1, probs = 0.90)
  threshold_d2 <- quantile(batch_data$intensity_method1, probs = 0.75)
  
  # Filter hotspots based on thresholds
  hot_spots_d1 <- batch_data %>% filter(intensity_method1 > threshold_d1)
  hot_spots_d2 <- batch_data %>% filter(intensity_method1 > threshold_d2)
  
  # Apply DBSCAN clustering for each threshold
  eps <- 1
  minPts <- 5
  db_d1 <- dbscan(hot_spots_d1[, c("x.pos.asec", "y.pos.asec")], eps = eps, minPts = minPts)
  db_d2 <- dbscan(hot_spots_d2[, c("x.pos.asec", "y.pos.asec")], eps = eps, minPts = minPts)
  
  # Add cluster info to the hotspots
  hot_spots_d1$cluster <- db_d1$cluster
  hot_spots_d2$cluster <- db_d2$cluster
  
  # Store the results for each batch
  hotspot_time_series_d1[[i]] <- hot_spots_d1
  hotspot_time_series_d2[[i]] <- hot_spots_d2
  hotspot_time_series[[i]] <-  rbind(
    hot_spots_d1 %>% mutate(hotspot_type = 'Very Hot Spot'),
    hot_spots_d2 %>% mutate(hotspot_type = 'Regional Hot Spot')
  )
}
create_hotspot_plot <- function(hot_spots, batch_number, hotspot_type_label) {
  ggplot(hot_spots, aes(x = x.pos.asec, y = y.pos.asec)) +
    geom_point(aes(color = as.factor(energy.kev), shape = as.factor(cluster), size = intensity_method1), alpha = 0.8) + 
    scale_shape_manual(values = c(16,17)) + # Shapes for different hotspot types
    scale_color_brewer(palette = "Spectral") + # Colors for energy bands +
    scale_size(range = c(2, 8)) +
    labs(title = paste("Hotspots (", hotspot_type_label, ") - Batch", batch_number),
         color = "Energy Band",
         shape = "Cluster",
         x = "X Position (arcsec)",
         y = "Y Position (arcsec)") +
    theme_minimal() +
    theme(legend.position = "right")
}
# Lists to store the plots for each threshold
plots_d1 <- list()
plots_d2 <- list()

# Loop through each batch and create the plots for both thresholds, this is to create a list of interactive plot(has nothing to do with the animation)
for (i in 1:11) {
  # Create and store the plot for threshold d1
  plots_d1[[i]] <- ggplotly(create_hotspot_plot(hotspot_time_series_d1[[i]], i, "Very Hot Spot"))
  # Create and store the plot for threshold d2
  plots_d2[[i]] <- ggplotly(create_hotspot_plot(hotspot_time_series_d2[[i]], i, "Regional Hot Spot"))
}
#ANIMATION FOR THE PLOTS
#VERY HOT SPOT ANIMATION

# Combine all batches into one data frame with a batch identifier
all_batches <- bind_rows(
  lapply(seq_along(hotspot_time_series_d1), function(i) {
    hotspot_time_series_d1[[i]] %>% 
      mutate(Batch = i)
  }),
  .id = "id"
)
# Create your ggplot
p <- ggplot(all_batches, aes(x = x.pos.asec, y = y.pos.asec, group = Batch)) +
  geom_point(aes(color = as.factor(energy.kev), size = intensity_method1), alpha = 0.8) +
  scale_size_continuous(range = c(2, 8)) +
  scale_color_brewer(palette = "Spectral") +
  theme_minimal() +
  labs(title = 'Very Hot Spot, Batch: {frame_time}', x = "X Position (arcsec)", y = "Y Position (arcsec)", color = "Energy Band", size = "Intensity") +
  theme(legend.position = "right")
# Animate the plot
anim <- p + 
  transition_time(Batch) +
  ease_aes('linear')
# Save the animation
animate(anim, duration = 20, fps = 1/2, width = 800, height = 600, renderer = gifski_renderer())
# You can save this to a file like this:
anim_save("hotspots_over_time_d1.gif", anim)

######### REGIONAL HOT SPOT ANIMATION
# Combine all batches into one data frame with a batch identifier
all_batches <- bind_rows(
  lapply(seq_along(hotspot_time_series_d2), function(i) {
    hotspot_time_series_d2[[i]] %>% 
      mutate(Batch = i)
  }),
  .id = "id"
)
# Create your ggplot
p <- ggplot(all_batches, aes(x = x.pos.asec, y = y.pos.asec, group = Batch)) +
  geom_point(aes(color = as.factor(energy.kev), size = intensity_method1), alpha = 0.8) +
  scale_size_continuous(range = c(2, 8)) +
  scale_color_brewer(palette = "Spectral") +
  theme_minimal() +
  labs(title = 'Regional Hot Spot, Batch: {frame_time}', x = "X Position (arcsec)", y = "Y Position (arcsec)", color = "Energy Band", size = "Intensity") +
  theme(legend.position = "right")
# Animate the plot
anim <- p + 
  transition_time(Batch) +
  ease_aes('linear')
# Save the animation
animate(anim, duration = 20, fps = 1/2, width = 800, height = 600, renderer = gifski_renderer())
# You can save this to a file like this:
anim_save("hotspots_over_time_d2.gif", anim)