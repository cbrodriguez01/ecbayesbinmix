set.seed(123)  # For reproducibility
n <- 50  # Number of rows
p <- 10  # Number of columns

# Generate a random binary matrix (0s and 1s)
matrix_data <- matrix(sample(0:1, n * p, replace = TRUE), nrow = n, ncol = p)

# Generate random cluster assignments
clusters <- sample(1:3, n, replace = TRUE)

# Order rows based on cluster assignment
ordered_matrix <- matrix_data[order(clusters), ]
library(ggplot2)
library(reshape2)  # For melting the matrix

# Convert the matrix to a data frame for ggplot
melted_matrix <- melt(ordered_matrix)
colnames(melted_matrix) <- c("Row", "Column", "Value")

# Create a factor for rows to maintain the order in the plot
melted_matrix$Row <- factor(melted_matrix$Row, levels = unique(melted_matrix$Row))

# Plot
ggplot(melted_matrix, aes(x = Column, y = Row, fill = factor(Value))) +
  geom_tile(color = "white") +  # Use white lines to separate the tiles
  scale_fill_manual(values = c("0" = "white", "1" = "black")) +  # Color scale
  labs(fill = "Value", x = "Column Index", y = "Cluster Ordered Rows") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x axis labels for better visibility
        legend.position = "bottom")


# Adding a new column for cluster to melted matrix
melted_matrix$Cluster <- rep(clusters[order(clusters)], each = p)

# Plot with facets
ggplot(melted_matrix, aes(x = Column, y = Row, fill = factor(Value))) +
  geom_tile(color = "white") +
  facet_grid(Cluster ~ ., scales = "free_y", space = "free") +  # Facet by cluster
  scale_fill_manual(values = c("0" = "white", "1" = "black")) +
  theme_minimal() +
  theme(strip.text.y = element_text(angle = 0))  # Rotate facet labels if necessary
