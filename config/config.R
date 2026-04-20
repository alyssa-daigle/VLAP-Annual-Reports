library(dotenv)

# Load once globally
if (file.exists(".env")) {
  load_dot_env(".env")
}

# Core config (SINGLE SOURCE OF TRUTH)
CONFIG <- list(
  PROJECT_PATH = Sys.getenv("PROJECT_PATH"),
  YEAR = Sys.getenv("YEAR"),
  INPUT_PATH = Sys.getenv("INPUT_PATH"),
  OUTPUT_BASE = Sys.getenv("OUTPUT_BASE"),
  TEMPLATE_PATH = Sys.getenv("TEMPLATE_PATH")
)

# Validation (fail fast once)
required <- names(CONFIG)
missing <- required[sapply(CONFIG, function(x) is.null(x) || x == "")]

if (length(missing) > 0) {
  stop("Missing .env values: ", paste(missing, collapse = ", "))
}

# Derived paths (also centralized)
CONFIG$OUTPUT_PATH <- file.path(CONFIG$OUTPUT_BASE, CONFIG$YEAR, "plots")
CONFIG$TABLE_PATH <- file.path(CONFIG$OUTPUT_BASE, CONFIG$YEAR, "tables")
CONFIG$REPORT_PATH <- file.path(
  CONFIG$OUTPUT_BASE,
  CONFIG$YEAR,
  "report-templates"
)
CONFIG$MK_PATH <- file.path(CONFIG$OUTPUT_BASE, CONFIG$YEAR, "mannkendall")

# Create dirs once
dir.create(CONFIG$OUTPUT_PATH, recursive = TRUE, showWarnings = FALSE)
dir.create(CONFIG$TABLE_PATH, recursive = TRUE, showWarnings = FALSE)
dir.create(CONFIG$REPORT_PATH, recursive = TRUE, showWarnings = FALSE)
dir.create(CONFIG$MK_PATH, recursive = TRUE, showWarnings = FALSE)

PROJECT_PATH <- CONFIG$PROJECT_PATH
YEAR <- CONFIG$YEAR
INPUT_PATH <- CONFIG$INPUT_PATH
OUTPUT_BASE <- CONFIG$OUTPUT_BASE
TEMPLATE_PATH <- CONFIG$TEMPLATE_PATH
TABLE_PATH <- CONFIG$TABLE_PATH
REPORT_PATH <- CONFIG$REPORT_PATH
MK_PATH <- CONFIG$MK_PATH
