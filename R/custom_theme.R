
# Load necessary libraries
library(ggplot2)

# Define a custom color palette
custom_palette <- function() {
  c("#008bb0", "#d06f1a", "#ffd200", "#006858",  "#695e4a", "#0033A1")
}


# Create a custom ggplot theme
custom_theme <- function() {
  theme_minimal(base_size = 15) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text = element_text(face = "bold"),
      legend.position = "right",
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank()
    )
}