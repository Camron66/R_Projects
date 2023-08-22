#=============================================================================
# PROGRAMMER: Camron
# PANTHER ID: 6187231
#
# CLASS: COP2210
# SECTION: RVC 1231
# SEMESTER: Spring 2023
# CLASSTIME: Online course
# CERTIFICATION: I understand FIU’s academic policies, and I certify that this
# work is my own and that none of it is the work of any other person.
#=============================================================================
rm(list=ls())
            #           #
            # Problem 1 #
            #           #

# Load required packages
library(triangle)

# Set seed for reproducibility
set.seed(123)

# Generate random variables for q1, q2, and p with triangle distribution

# Generate random variables for s with exponential distribution

# Create data frame "inputs" with the generated random variables
inputs <- data.frame(q1 = rtriangle(2000, a = 0, b = 1500, c = 1200), 
                     q2 = rtriangle(2000, a = 0, b = 3500, c = 1000), 
                     p = rtriangle(2000, a = 10, b = 17.50, c = 12.50), 
                     s = rexp(2000, rate = 10))

            #           #
            # Problem 2 #
            #           #

# Set up the plotting layout
par(mfrow=c(4, 1), mar=c(2,2,2,2))

# Plot histograms of each column in inputs data frame
hist(inputs$q1, main="Histogram of q1", xlab="q1")
hist(inputs$q2, main="Histogram of q2", xlab="q2")
hist(inputs$p, main="Histogram of p", xlab="p")
hist(inputs$s, main="Histogram of s", xlab="s")

            #           #
            # Problem 3 #
            #           #
  
outputs <- data.frame(value= (2700 - inputs$q1 - inputs$q2)*inputs$p - (inputs$s * inputs$p))  

            #           #
            # Problem 4 #
            #           #
# Set the number of iterations for the Monte Carlo simulation
num_iterations <- 1000

# Perform Monte Carlo simulation
for (i in 1:num_iterations) {
  # Generate random variables for q1, q2, p, s
  inputs$q1 <- rtriangle(nrow(inputs), a = 0, b = 1500, c = 1200)
  inputs$q2 <- rtriangle(nrow(inputs), a = 0, b = 3500, c = 1000)
  inputs$p <- rtriangle(nrow(inputs), a = 10, b = 17.50, c = 12.50)
  inputs$s <- rexp(nrow(inputs), rate = 10)
  
  # Update the "value" column in outputs data frame with the calculated results
  outputs$value <- (2700 - inputs$q1 - inputs$q2) * inputs$p - (inputs$s * inputs$p)
}

# Print the first few rows of the updated outputs data frame
              #           #
              # Problem 5 #
              #           #

hist(outputs$value, main="Histogram of outputs$value", xlab="outputs$value")

              #           #
              # Problem 6 #
              #           #
ecdf_outputs <- ecdf(outputs$value)
plot(ecdf_outputs, main="Empirical CDF of outputs$value", xlab="outputs$value", ylab="CDF")

              #           #
              # Problem 7 #
              #           #

percentiles <- seq(0, 100, by=10)
percentile_values <- quantile(outputs$value, probs=percentiles/100)
cat("P0: ", percentile_values[1], "\n")
cat("P10: ", percentile_values[2], "\n")
cat("P20: ", percentile_values[3], "\n")
cat("P30: ", percentile_values[4], "\n")
cat("P40: ", percentile_values[5], "\n")
cat("P50: ", percentile_values[6], "\n")
cat("P60: ", percentile_values[7], "\n")
cat("P70: ", percentile_values[8], "\n")
cat("P80: ", percentile_values[9], "\n")
cat("P90: ", percentile_values[10], "\n")
cat("P100: ", percentile_values[11], "\n")
              #           #
              # Problem 8 #
              #           #

p20 <- ecdf_outputs(percentile_values[3])
p80 <- ecdf_outputs(percentile_values[9])
p_interval <- p80 - p20
cat("P20 to P80 interval values from empirical CDF: ", p_interval, "\n")

              #           #
              # Problem 9 #
              #           #

n_samples <- 100
n_realizations <- 250

storage <- matrix(0, nrow = n_samples, ncol = n_realizations)

for (i in 1:n_samples) {
  storage[i, ] <- (2700 - rtriangle(n_realizations, 0, 1500, 1200) - rtriangle(n_realizations, 0, 3500, 1000))* rtriangle(n_realizations, 10, 17.5, 12.5)- (rexp(n_realizations, 10) * rtriangle(n_realizations, 10, 17.5, 12.5))
}

              #            #
              # Problem 10 #
              #            #

# Convert the storage matrix into a data frame
storage_df <- as.data.frame(storage)

              #            #
              # Problem 11 #
              #            #

# Create a data frame to store the means of each column in the storage data frame
cltData <- data.frame(colMeans(storage_df))
# Calculate the means of each column in the storage data frame and store in cltData

              #            #
              # Problem 12 #
              #            #

# Plot histograms of the data within cltData
par(mfrow=c(1,1), mar=c(2,2,2,2))
hist(cltData$colMeans.storage_df., main="Histogram of Means", xlab="Mean")

              #            #
              # Problem 13 #
              #            #

# Perform normality test on cltData
normal_test <- shapiro.test(cltData$colMeans.storage_df.)

              #            #
              # Problem 14 #
              #            #

# Check if the data within cltData is normally distributed
if(normal_test$p.value > 0.05){
  cat("The data in cltData is normally distributed.\n")
} else {
  cat("The data in cltData is not normally distributed.\n")
}

              #            #
              # Problem 15 #
              #            #

# Calculate the 80% confidence interval
confidence_interval <- quantile(cltData$colMeans.storage_df., probs = c(0.1, 0.9))
lower_bound <- confidence_interval[1]
upper_bound <- confidence_interval[2]
cat("80% Confidence Interval: [", lower_bound, ", ", upper_bound, "]\n")

