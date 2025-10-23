library(tidyverse)
library(readr)
library(dplyr)

# step 1 builder function 

label_randomly <- function(n_points, n_clusters) {
  # assign cluster labels randomly
  labels <- ((1:n_points - 1) %% n_clusters) + 1
  sample(labels, n_points, replace = FALSE)
}
  
  # step 2 builder function 
  
get_cluster_means <- function(data, labels) {
  data %>%
    mutate(label = labels) %>%
    group_by(label) %>%
    summarise(across(everything(), mean), .groups = "drop") %>%
    arrange(label)
}

#step 3 builder function 

assign_cluster <- function(data, means) {
  data_mat <- as.matrix(data)
  means_mat <- as.matrix(means %>% select(-label))
  
  dist_mat <- outer(
    1:nrow(data_mat), 1:nrow(means_mat),
    Vectorize(function(i, j) sum((data_mat[i, ] - means_mat[j, ])^2))
  )
  
  labels <- means$label[apply(dist_mat, 1, which.min)]
  labels
}

#step 4 builder function 

kmeans_done <- function(old_means, new_means, eps = 1e-6) {
  om <- as.matrix(old_means %>% select(-label))
  nm <- as.matrix(new_means %>% select(-label))
  
  m <- mean(sqrt(rowSums((om - nm)^2)))
  m < eps
}

#Final 
mykmeans <- function(data, n_clusters, eps = 1e-6) {
  labels <- label_randomly(nrow(data), n_clusters)
  old_means <- get_cluster_means(data, labels)
  done <- FALSE
  
  while (!done) {
    labels <- assign_cluster(data, old_means)
    new_means <- get_cluster_means(data, labels)
    
    if (kmeans_done(old_means, new_means, eps)) {
      done <- TRUE
    } else {
      old_means <- new_means
    }
  }
  
  list(labels = labels, means = new_means)
}

#Question 2 

#a)
voltages_df <- read_csv("~/Desktop/BIOS512/Homeworks/Homework 8/voltages_df.csv")

view(voltages_df)

#b)
results <- mykmeans(voltages_df, n_clusters = 3)
print(results$labels)
print(results$means)

#c)
voltages_matrix <- as.matrix(voltages_df)
results2 <- kmeans(voltages_matrix, centers = 3)

print(results2$labels)   
print(results2$cluster)
print(results2$centers)


