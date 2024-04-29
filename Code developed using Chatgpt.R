# Load necessary libraries
library(ggplot2)

# Define standard curve data
standard_curve <- data.frame(
  Concentration = c(0, 0.025, 0.125, 0.25, 0.5, 1, 1.5, 2),
  Absorbance = c(0, 0.004, 0.028, 0.058, 0.118, 0.215, 0.294, 0.418),
  Type = "Standard",
  Protein = NA
)

# Define absorbance values of unknown proteins
unknown_absorbance <- c(0.1712, 0.1865, 0.237, 0.2345)

# Interpolate concentrations for unknown proteins
unknown_concentrations <- approx(standard_curve$Absorbance, standard_curve$Concentration, xout = unknown_absorbance)$y

# Create dataframe for unknown proteins with calculated concentrations
unknown_proteins <- data.frame(
  Concentration = unknown_concentrations,
  Absorbance = unknown_absorbance,
  Type = "Unknown",
  Protein = c("Male control", "Female control", "Male dosed", "Female dosed")
)

# Combine data frames
all_data <- rbind(standard_curve, unknown_proteins)

# Fit linear model to standard data
lm_model <- lm(Absorbance ~ Concentration, data = standard_curve)

# Define colors for unknown proteins
unknown_colors <- c("Male control" = "blue", "Female control" = "red", "Male dosed" = "green", "Female dosed" = "purple")

# Custom legend data
legend_data <- data.frame(
  Protein = c("Standard", "Male control", "Female control", "Male dosed", "Female dosed"),
  Color = c("black", unknown_colors)
)

# Plot
p <- ggplot(all_data, aes(x = Concentration, y = Absorbance, shape = Type)) +
  geom_point(data = all_data[all_data$Type == "Standard", ], aes(color = "Standard", fill = "Standard"), size = 3) +  # Standard data points
  geom_point(data = unknown_proteins, aes(color = Protein, fill = Protein), size = 3) +  # Unknown data points
  geom_smooth(data = standard_curve, aes(group = 1), method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("Standard", as.character(legend_data$Color)), name = "Protein", labels = c("Standard", legend_data$Protein)) +
  scale_color_manual(values = c("black", unknown_colors), guide = "none") +  # Remove legend for "Data Type"
  scale_shape_manual(values = c(16, 17), guide = "none") +
  labs(x = "Concentration (mg/ml)", y = "Absorbance", title = "BCA Assay Data") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.title.align = 0.5,  # Center legend title
        legend.spacing.y = unit(0.2, "cm")) +  # Increase spacing between legend items
  guides(shape = guide_legend(override.aes = list(color = "black")),
         fill = guide_legend(override.aes = list(shape = 15, color = legend_data$Color)))  # Custom legend for colors and names

# Add custom legend
p <- p + scale_fill_manual(values = as.character(legend_data$Color),
                           name = "Protein",
                           labels = legend_data$Protein,
                           guide = guide_legend(override.aes = list(shape = 15)))

print(p)

