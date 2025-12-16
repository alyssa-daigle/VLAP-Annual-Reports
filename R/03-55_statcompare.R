library(tidyverse)

compare_reg_mk_trends <- function(
  tables_path = "tables",
  reg_pattern = "^REG_TrendSummary_",
  mk_pattern = "^MK_TrendSummary_",
  output_path = "tables"
) {
  # ---- read regression summaries ----
  reg_files <- list.files(
    tables_path,
    pattern = reg_pattern,
    full.names = TRUE
  )

  reg_trends <- map_dfr(
    reg_files,
    ~ {
      station <- str_remove(basename(.x), reg_pattern) |>
        str_remove("\\.csv")

      read_csv(.x, show_col_types = FALSE) |>
        mutate(
          stationid = station,
          method = "Regression"
        )
    }
  )

  # ---- read MK summaries ----
  mk_files <- list.files(
    tables_path,
    pattern = mk_pattern,
    full.names = TRUE
  )

  mk_trends <- map_dfr(
    mk_files,
    ~ {
      station <- str_remove(basename(.x), mk_pattern) |>
        str_remove("\\.csv")

      read_csv(.x, show_col_types = FALSE) |>
        mutate(
          stationid = station,
          method = "Mann-Kendall"
        )
    }
  )

  # ---- combine and reshape ----
  comparison <- bind_rows(reg_trends, mk_trends) |>
    select(stationid, PARAMETER, method, TREND) |>
    pivot_wider(
      names_from = method,
      values_from = TREND
    ) |>
    mutate(
      agreement = case_when(
        is.na(Regression) | is.na(`Mann-Kendall`) ~ NA_character_,
        Regression == `Mann-Kendall` ~ "Agree",
        TRUE ~ "Disagree"
      )
    )

  # ---- save full comparison ----
  write_csv(
    comparison,
    file.path(output_path, "TrendComparison_REG_vs_MK.csv")
  )

  # ---- summary statistics ----
  summary_table <- comparison |>
    filter(!is.na(agreement)) |>
    count(PARAMETER, agreement) |>
    pivot_wider(
      names_from = agreement,
      values_from = n,
      values_fill = 0
    ) |>
    mutate(
      total = Agree + Disagree,
      percent_agree = round(Agree / total * 100, 1)
    )

  write_csv(
    summary_table,
    file.path(output_path, "TrendComparison_Summary.csv")
  )

  return(list(
    full_comparison = comparison,
    summary = summary_table
  ))
}
