custom_theme <- function() {
  library(ggplot2)

  list(
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = c(-0.05, 1.2),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.01, "cm"), # less vertical spacing in legend
      legend.margin = margin(t = 2, r = 2, b = 2, l = 2), # smaller legend margin
      legend.text = element_text(size = 8),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 20), # shrink plot margins
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 20)
      ),
      panel.border = element_blank(), # removes all panel borders
      axis.line = element_line(color = "gray40")
    )
  )
}

pH_theme <- function() {
  library(ggplot2)

  list(
    theme()
  )
}
