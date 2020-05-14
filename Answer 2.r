# Importing the necessary packages
library(ggplot2)
library(dplyr)
library(ggpubr)
library(mclust)

# Reading the data and processing it
original_wines = read.csv("winemag-data-130k-v2.csv")
wines_usa = filter(original_wines, (country == "US"))
wines_usa = wines_usa[c("points","price")]
wines_usa = na.omit(wines_usa)

# Setting the seed for reproducability
set.seed(100)

# Applying clustering on the data
fit <- Mclust(wines_usa)

# For analysis of the model fitted
print(fit)
summary(fit)

# Plotting the BIC graph
plot(fit, what = "BIC")

# Analyzig the BIC for optimum model selection
fit$BIC

# Clustering using the optimal model
fit2 <- Mclust(wines_usa, G = 9, modelNames = "VVV")

# Visualizing and analyzing 
summary(fit2)
plot(fit2, what = "classification")
plot(fit2, what = "classification", ylim = c(0, 200))
plot(fit2, what = "uncertainty",  ylim = c(0, 200))
fit2$parameters$pro ## size of each group
fit2$parameters$mean ## cluster means

# Create the dataframe with classification
wines_classified = data.frame(fit2$classification, fit2$data)

# Visualizing the mean rating per cluster
cluster_numbers = data.frame(fit2$classification, fit2$data)
final_dataset = merge(wines_usa, cluster_numbers,by=0)[-1]
avg_size = data.frame(size=tapply(final_dataset$points.x, final_dataset$fit2.classification, length))
avg_size$row = row.names(avg_size)
max_wines_in_cluster <- max(avg_size$size)
avg_wines_in_a_cluster <- mean(avg_size$size)

avg_mean_rating = data.frame(mean_rating = tapply(final_dataset$points.x, final_dataset$fit2.classification, mean))
avg_mean_rating$row = row.names(avg_mean_rating)
avg = merge(avg_size,avg_mean_rating, by.x="row", by.y="row")

ggplot(avg, aes(as.numeric(factor(row)), mean_rating))+
  geom_point(aes(size = size, color = row))+
  scale_x_continuous(breaks = row_number(avg$row))+
  geom_hline(aes(yintercept = mean(mean_rating)), color="black", linetype="dashed") + 
  xlab("Cluster number")+
  ylab("Mean rating per cluster")