theme_chl_tp_secchi <- function() {
  list(
    theme(
      panel.grid = element_blank(), # no plot grid
      legend.position = "right", # position of legend on plot
      legend.background = element_blank(), #element_rect(fill = "transparent", size = 0.5),
      legend.key = element_rect(fill = "white"),
      legend.spacing.y = unit(0.01, "cm"),
      legend.margin = margin(t = 1, r = 1, b = 1, l = 1), # top right bottom left
      legend.text = element_text(size = 8),
      legend.key.width = unit(0.3, "cm"),
      legend.key.height = unit(0.3, "cm"),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 20),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 20),
        size = 13.5
      ),
      axis.line = element_line(color = "gray40"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 10, face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
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
      legend.text = element_text(size = 10),
      plot.margin = margin(t = 20, r = 5, b = 10, l = 20),
      plot.title = element_text(
        hjust = 0.5,
        face = "bold",
        margin = margin(b = 20),
        size = 13
      ),
      panel.border = element_blank(),
      axis.line = element_line(color = "gray40"),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8)
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
        size = 13
      ),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      legend.position = "right",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 9)
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
      legend.position = "none", # hide legend here
      plot.margin = margin(t = 0, r = 10, b = 10, l = 10),
      plot.title = element_text(
        hjust = 0.5,
        vjust = -2.0,
        face = "bold",
        margin = margin(b = 20),
        size = 13
      ),
      panel.border = element_rect(size = 1),
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.ticks.y = element_blank()
    )
  )
}

theme_plankton_legend <- function() {
  list(
    theme(
      legend.text = element_text(size = 10, face = "bold"),
      legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(t = 1, r = 10, b = 1, l = 1), # top right bottom left
    )
  )
}

#plankton color palette and renaming
algae_colors <- c(
  "UNKNOWN PHYTO" = "#772c2a",
  "GREEN" = "#2c4d75",
  "GOLDEN-BROWN" = "#4bacc6",
  "EUGLENOID" = "#9bbb59",
  "DINOFLAGELLATE" = "#e46c0a",
  "DIATOM" = "#7f7f7f",
  "CYANOBACTERIA" = "#604a7b",
  "CRYPTOMONAD" = "#4f6228",
  "XANTHOPHYTE" = "#c0504d"
)

algae_labels <- c(
  "UNKNOWN PHYTO" = "Unknown",
  "GREEN" = "Greens",
  "GOLDEN-BROWN" = "Golden-Brown",
  "EUGLENOID" = "Euglenoids",
  "DINOFLAGELLATE" = "Dinoflagellates",
  "DIATOM" = "Diatoms",
  "CYANOBACTERIA" = "Cyanobacteria",
  "CRYPTOMONAD" = "Cryptomonads",
  "XANTHOPHYTE" = "Xanthophytes"
)
