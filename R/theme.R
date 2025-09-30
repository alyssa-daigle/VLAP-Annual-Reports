theme_chl_tp_secchi <- function() {
  list(
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = c(1.0, 1.1),
      legend.background = element_rect(fill = "transparent", size = 0.5),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
      legend.text = element_text(size = 8),
      legend.key.width = unit(0.3, "cm"),
      legend.key.height = unit(0.3, "cm"),
      plot.margin = margin(t = 35, r = 40, b = 10, l = 20),
      plot.title = element_text(
        hjust = 0.05,
        face = "bold",
        margin = margin(b = 20)
      ),
      axis.line = element_line(color = "gray40")
    )
  )
}
