#GETTING RID OF V1, V2, V3,..., V21 COLUMN NAMES
header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}
data2 = header.true(data1)
#DEVELOP METHOD 2
# Load necessary libraries
library(dplyr)

# Sample data for illustration (replace this with your actual data)
data2 <- dataT1

# Define a function to calculate intensity using Method 2
calculate_intensity_method2 <- function(duration, energy_band) {
  # Define weightage for duration and energy band (you can adjust these weights)
  duration_weight <- 0.6
  energy_weight <- 0.4
  # Mapping energy bands to numerical values (midpoints of the bands)
  energy_values <- c(9, 18.5, 37.5, 75, 200, 550, 3900, 13500) 
  # Calculate intensity using weighted sum of duration and energy values
  intensity <- (duration * duration_weight) + 
    ((energy_values[match(energy_band, c('6-12', '12-25', '25-50', '50-100', '100-300', '300-800', '800-7000', '7000-20000'))] * energy_weight))
  
  return(intensity)
}
# Apply the function to calculate intensity for each row in the dataset
data2 <- data2 %>%
  mutate(intensity_method2 = calculate_intensity_method2(as.numeric(duration.s), energy.kev))

# Print the updated dataset with the calculated intensity using Method 2
print(data2)
#CREATE DENSITY MAP FOR MONTHS 1+2+3+4 AND MONTHS 21+22+23+24
# Load necessary libraries
library(ggplot2)

# Assuming your dataset is stored in a variable named 'data'
# Replace 'data' with the actual name of your dataset variable if it's different
#STEP 1
# Filter data for months 1+2+3+4 (Batch 1)
batch1_Map <- data2 %>%
  filter(month >= 1 & month <= 4 & year <= 2004)

# Filter data for months 21+22+23+24 (Batch 11)
batch2_Map <- data2 %>%
  filter(month >= 9 & month <= 12 & year >= 2005)
#STEP 2
# Calculate intensity for Batch 1 and Batch 11 using Method 2
batch1_Map <- batch1_Map %>%
  mutate(intensity_method2 = calculate_intensity_method2(as.numeric(duration.s), energy.kev))
batch2_Map <- batch2_Map %>%
  mutate(intensity_method2 = calculate_intensity_method2(as.numeric(duration.s), energy.kev))
#STEP 3
batch1_Map$x.pos.asec = as.numeric(batch1_Map$x.pos.asec)
batch1_Map$y.pos.asec = as.numeric(batch1_Map$y.pos.asec)
batch2_Map$x.pos.asec = as.numeric(batch2_Map$x.pos.asec)
batch2_Map$y.pos.asec = as.numeric(batch2_Map$y.pos.asec)
# Create intensity maps for Batch 1 using Method 2
heatmap_batch1 <- ggplot(batch1_Map, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_method2)) +
  geom_tile() +
  labs(title = "Intensity Map: Months 1+2+3+4 (Method 2)", x = "X Position", y = "Y Position")
heatmap_batch2 <- ggplot(batch2_Map, aes(x = x.pos.asec, y = y.pos.asec, fill = intensity_method2)) +
  geom_tile() +
  labs(title = "Intensity Map: Months 1+2+3+4 (Method 2)", x = "X Position", y = "Y Position")
#Printing plots
print(heatmap_batch1)
print(heatmap_batch2)