theme_chl_tp_secchi <- function() {
  list(
    theme(
      panel.grid = element_blank(),
      legend.background = element_blank(),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.01, "cm"),
      legend.margin = margin(t = 1, r = 1, b = 1, l = 1),
      legend.text = element_text(size = 8, family = "Calibri"),
      legend.key.width = unit(0.3, "cm"),
      legend.key.height = unit(0.3, "cm"),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 20),
      plot.title = element_text(
        hjust = 0.4,
        face = "bold",
        margin = margin(b = 10),
        size = 14,
        family = "Calibri"
      ),
      axis.line = element_line(color = "gray40"),
      axis.title.x = element_text(size = 12, face = "bold", family = "Calibri"),
      axis.title.y = element_text(size = 12, face = "bold", family = "Calibri"),
      axis.text.y = element_text(size = 9, family = "Calibri"),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8,
        family = "Calibri"
      )
    )
  )
}

theme_pH_conduc <- function() {
  list(
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.01, "cm"),
      legend.margin = margin(t = 2, r = 2, b = 2, l = 1),
      legend.text = element_text(size = 10, family = "Calibri"),
      plot.margin = margin(t = 20, r = 5, b = 10, l = 20),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 20),
        size = 14,
        family = "Calibri"
      ),
      panel.border = element_blank(),
      axis.line = element_line(color = "gray40"),
      axis.title.x = element_text(size = 12, face = "bold", family = "Calibri"),
      axis.title.y = element_text(size = 12, face = "bold", family = "Calibri"),
      axis.text.y = element_text(size = 10, family = "Calibri"),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8,
        family = "Calibri"
      )
    ),
    scale_fill_manual(values = c("pH" = "white")),
    scale_color_manual(values = c("cond" = "red3"))
  )
}

theme_chloride <- function() {
  list(
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.01, "cm"),
      legend.margin = margin(t = 2, r = 2, b = 2, l = 1),
      legend.text = element_text(size = 10, family = "Calibri"),
      plot.margin = margin(t = 20, r = 5, b = 10, l = 20),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 20),
        size = 13,
        family = "Calibri"
      ),
      panel.border = element_blank(),
      axis.line = element_line(color = "gray40"),
      axis.title.x = element_text(size = 12, face = "bold", family = "Calibri"),
      axis.title.y = element_text(size = 12, face = "bold", family = "Calibri"),
      axis.text.y = element_text(size = 10, family = "Calibri"),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 8,
        family = "Calibri"
      )
    )
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
        size = 14,
        family = "Calibri"
      ),
      axis.title.x = element_text(size = 12, face = "bold", family = "Calibri"),
      axis.title.y = element_text(size = 12, face = "bold", family = "Calibri"),
      axis.text.x = element_text(size = 10, family = "Calibri"),
      axis.text.y = element_text(size = 10, family = "Calibri"),
      legend.position = "right",
      legend.text = element_text(size = 10, face = "bold", family = "Calibri"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      plot.margin = margin(5, 5, 5, 5)
    )
  )
}

theme_plankton <- function() {
  list(
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(color = "whitesmoke"),
      panel.grid.minor.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10),
      plot.title = element_text(
        hjust = 0.5,
        #vjust = -2.0,
        face = "bold",
        margin = margin(b = 20),
        size = 14,
        family = "Calibri"
      ),
      panel.border = element_rect(size = 1),
      axis.title.x = element_text(size = 12, face = "bold", family = "Calibri"),
      axis.title.y = element_text(size = 14, face = "bold", family = "Calibri"),
      axis.text.y = element_text(size = 10, family = "Calibri"),
      axis.text.x = element_text(size = 10, family = "Calibri"),
      axis.ticks.y = element_blank()
    )
  )
}

theme_plankton_legend <- function() {
  list(
    theme(
      legend.text = element_text(size = 10, family = "Calibri"),
      legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(t = 1, r = 10, b = 1, l = 1)
    )
  )
}
