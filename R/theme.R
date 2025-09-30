theme_chl_tp_secchi <- function() {
  list(
    theme(
      panel.grid = element_blank(), # no plot grid
      legend.position = c(0.98, 1.175), # position of legend on plot
      legend.background = element_blank(), #element_rect(fill = "transparent", size = 0.5),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.01, "cm"),
      legend.margin = margin(t = 1, r = 1, b = 1, l = 1), # top right bottom left
      legend.text = element_text(size = 8),
      legend.key.width = unit(0.3, "cm"),
      legend.key.height = unit(0.3, "cm"),
      plot.margin = margin(t = 35, r = 40, b = 10, l = 20),
      plot.title = element_text(
        hjust = 0.05,
        face = "bold",
        margin = margin(b = 20)
      ),
      axis.line = element_line(color = "gray40"),
      axis.title.x = element_text(size = 8, face = "bold"),
      axis.title.y = element_text(size = 8, face = "bold"),
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
    )
  )
}

theme_pH_conduc <- function() {
  list(
    theme(
      panel.grid = element_blank(),
      legend.position = c(-0.05, 1.2),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.01, "cm"),
      legend.margin = margin(t = 2, r = 2, b = 2, l = 2),
      legend.text = element_text(size = 8),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 20),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 20)
      ),
      panel.border = element_blank(),
      axis.line = element_line(color = "gray40"),
      axis.title.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
    ),
    scale_fill_manual(values = c("pH" = "white")),
    scale_color_manual(values = c("cond" = "red3"))
  )
}

theme_temp_DO <- function() {
  list(
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "whitesmoke"),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        size = 14
      ),
      axis.title.x = element_text(size = 10, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )
  )
}
