install.packages("DDoutlier") # for LDOF distance
library(DDoutlier)
library(readr)
RBWD_ALL <- read_csv("Basel_Weather.csv")
RBWD<- RBWD_ALL[, c("DATE", "precipitation", "sunshine", "temp_mean", "humidity")]

#a
#ldof function
function (dataset, k = 5) 
{
  n <- nrow(dataset)
  dataset <- as.matrix(dataset)
  if (!is.numeric(k)) {
    stop("k input must be numeric")
  }
  if (k >= n || k < 1) {
    stop("k input must be less than number of observations and greater than 0")
  }
  if (!is.numeric(dataset)) {
    stop("dataset input is not numeric")
  }
  distMatrix <- as.matrix(dist(dataset))
  LDOF <- NULL
  for (i in 1:n) {
    vector <- distMatrix[i, ]
    sVector <- sort(vector)
    np <- as.numeric(names(sVector))[2:(k + 1)]
    dxp <- sum(sVector[2:(k + 1)])/k
    Dxp <- sum(distMatrix[np, np])/(k * (k - 1))
    LDOF[i] <- dxp/Dxp
  }
  return(LDOF)
}
#b in report

#c
scaled_RBWD <- cbind(RBWD$DATE,scale(RBWD[,-1]))
set.seed(123)
#k = 5
precipitation_outlier_score_5 <- LDOF(dataset = scaled_RBWD[,-1], k = 5)

#create a new dataset that include each new OLS column add
RBWD_with_OLS_K5 <- cbind(RBWD, OLS = precipitation_outlier_score_5)

#k = 10
precipitation_outlier_score <- LDOF(dataset = scaled_RBWD[,-1], k = 10)

#create a new dataset that include each new OLS column add
RBWD_with_OLS_K10 <- cbind(RBWD, OLS = precipitation_outlier_score)

#k = 15
precipitation_outlier_score_15 <- LDOF(dataset = scaled_RBWD[,-1], k = 15)

#create a new dataset that include each new OLS column add
RBWD_with_OLS_K15 <- cbind(RBWD, OLS = precipitation_outlier_score_15)

#k = 20
precipitation_outlier_score_20 <- LDOF(dataset = scaled_RBWD[,-1], k = 20)

#create a new dataset that include each new OLS column add
RBWD_with_OLS_K20 <- cbind(RBWD, OLS = precipitation_outlier_score_20)

#d) Check min max of precipitation, sunshine temp_mean or humidity for finding
#a reason why outlier is "an outlier"
# Find the highest value in each column
max_values <- apply(RBWD[, -1], 2, max)

# Find the lowest value in each column
min_values <- apply(RBWD[, -1], 2, min)

# Print or view the results
cat("Highest values in each column:\n")
print(max_values)

cat("\nLowest values in each column:\n")
print(min_values)


