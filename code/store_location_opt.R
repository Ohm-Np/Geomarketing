### Load required libraries

library(sf)
library(dplyr)
library(caret)
library(randomForest)
library(geosphere)

--------------------------------------------------------------------------------

  ### Load and prepare data

  sales_data <-
  read.csv("data/sales_data.csv")
demographic_data <-
  read.csv("data/demographic_data.csv")
geodata <-
  read_sf("data/german_cities.gdb")

# Merge sales and demographic data
data <-
  merge(sales_data, demographic_data, by = "city_name")

# Select the desired columns and rename them
data <- data %>%
  select(city_name,
    latitude = latitude.x, longitude = longitude.x, store_id,
    sale_date, sales_amount, population_density, income_level, average_age
  )

# Convert to spatial data frame
data_sf <-
  st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326)

--------------------------------------------------------------------------------

  ### Feature engineering

  dist_to_existing_stores <-
  function(lat, lon, existing_locations) {
    distances <- distHaversine(cbind(lon, lat), existing_locations)
    return(mean(distances))
  }

# Add latitude and longitude as separate columns to the sf object
coords <- st_coordinates(data_sf)
data_sf$longitude <- coords[, "X"]
data_sf$latitude <- coords[, "Y"]

existing_locations <- st_coordinates(data_sf)
data_sf <- data_sf %>%
  rowwise() %>%
  mutate(
    distance_to_existing_stores =
      dist_to_existing_stores(latitude, longitude, existing_locations)
  )

--------------------------------------------------------------------------------

  ### Model building

  # Prepare data for modeling
  features <- data_sf %>%
  select(distance_to_existing_stores, population_density, income_level)

sales <- data_sf$sales_amount

# split data into training and test sets
set.seed(42)
train_index <- createDataPartition(sales, p = 0.7, list = FALSE)
train_data <- features[train_index, ]
train_sales <- sales[train_index]

test_data <- features[-train_index, ]
test_sales <- sales[-train_index]

# train random forest model
# remove non numeric column from train data

train_data_nosf <-
  st_drop_geometry(train_data)


model <- randomForest(
  x = train_data_nosf,
  y = train_sales,
  ntree = 100,
  importance = TRUE
)

--------------------------------------------------------------------------------

  ### Optimization

  # example potential locations
  # Hanover, Nuremberg, Dresden
  potential_locations <- data.frame(
  latitude = c(52.3759, 49.4543, 51.0504),
  longitude = c(9.7320, 11.0746, 13.7373),
  population_density = c(4125, 4325, 3925),
  income_level = c(46000, 48000, 44000)
)

potential_locations_new <- potential_locations %>%
  rowwise() %>%
  mutate(distance_to_existing_stores = dist_to_existing_stores(
    latitude, longitude,
    existing_locations
  ))

predictions <- predict(model, newdata = as.data.frame(potential_locations_new))
optimal_index <- which.max(predictions)

optimal_location <- potential_locations_new[optimal_index, ]
print(optimal_location)

--------------------------------------------------------------------------------

  ### Validation

  # predict on data set
  predictions <- predict(model, newdata = as.data.frame(test_data))

# calculate accuracy metrics
mse <- mean((predictions - test_sales)^2)
rmse <- sqrt(mse)
cat("Root Mean Squared Error:", rmse)


--------------------------------------------------------------------------------

  ## Print the optimal location with expected sales
  optimal_location$predicted_sales <-
  predict(model, newdata = as.data.frame(optimal_location))

print(optimal_location)
