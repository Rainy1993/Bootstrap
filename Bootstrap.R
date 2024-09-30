rm(list = ls(all = TRUE))
graphics.off()

# Load necessary library
library(boot)

path <- "/Users/ruting/Documents/Github/Bootstrap"
setwd(path)

# Load the dataset
data(trees)
# View the first few rows of the dataset
head(trees)

# Summary statistics of the dataset
summary(trees)

# Basic plot to visualize the relationships between variables
pairs(trees)

png("TreeVolumeRelationship.png", width = 800, height = 600, bg = "transparent")

# Plot Height vs. Volume
plot(trees$Height, trees$Volume,
     main = "Black Cherry Tree Volume Relationship",
     xlab = "Height (feet)",
     ylab = "Volume (cubic feet)",
     pch = 19, col = "blue")

dev.off()

# Define a function to calculate R-squared for different variable combinations
calc_r_squared <- function(data, indices, formula) {
  # Get the bootstrap sample
  sample_data <- data[indices, ]
  
  # Fit the linear model based on the provided formula
  model <- lm(formula, data = sample_data)
  return(summary(model)$r.squared)
}

# Number of bootstrap samples
n_bootstrap <- 1000

# Formula for the model
formula <- Volume ~ Height

# Perform bootstrap to calculate R-squared values
boot_res <- boot(data = trees, statistic = calc_r_squared, R = n_bootstrap, formula = formula)

png("Histgram_t.png", width = 800, height = 600, bg = "transparent")
# Histogram of the bootstrapped R-squared values
hist(boot_res$t, breaks = 30, main = "Histogram of Bootstrapped R-squared",
     xlab = "R-squared", col = "lightgray", border = "black", prob = TRUE)

coef_interval <- quantile(boot_res$t, probs = c(0.05, 0.95))


# Add density curve
lines(density(boot_res$t), col = "blue", lwd = 2)
dev.off()

png("QQ.png", width = 800, height = 600, bg = "transparent")
# Q-Q plot of the bootstrapped R-squared values
qqnorm(boot_res$t, main = "Q-Q Plot of Bootstrapped R-squared")
qqline(boot_res$t, col = "red", lwd = 2)
dev.off()




