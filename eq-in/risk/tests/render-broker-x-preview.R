library(DBI)
library(RSQLite)
library(rmarkdown)
library(knitr)
library(htmltools)

args <- commandArgs(trailingOnly = TRUE)
if(length(args) != 2) stop("Usage: Rscript tests/render-broker-x-preview.R RISK_DIRECTORY OUTPUT_DIRECTORY")
root <- normalizePath(args[1], mustWork = TRUE)
out <- args[2]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out, mustWork = TRUE)
source(file.path(root, "broker-x-statements.R"))
dbPath <- cnbcStatementsDbPath()
if(!file.exists(dbPath)) stop("Configured CNBC database is missing; set STOCKVIZ_X_STREAM_DB explicitly.")

# Capture database content and sidecar inventory before and after real rendering.
feedState <- function(path) {
  con <- dbConnect(SQLite(), path, flags = RSQLite::SQLITE_RO, synchronous = NULL)
  on.exit(dbDisconnect(con))
  list(md5 = unname(tools::md5sum(path)),
    sidecars = file.exists(paste0(path, c("-wal", "-shm", "-journal"))),
    sidecarMd5 = unname(tools::md5sum(paste0(path, c("-wal", "-shm", "-journal"))[
      file.exists(paste0(path, c("-wal", "-shm", "-journal")))])),
    counts = dbGetQuery(con, "SELECT (SELECT COUNT(*) FROM tweets) tweets,
      (SELECT COUNT(*) FROM statements) statements,
      (SELECT COUNT(*) FROM statement_subjects) subjects,
      (SELECT COUNT(*) FROM ocr_results) ocr"))
}

before <- feedState(dbPath)
template <- readLines(file.path(root, "rp-eq-in-risk.Rmd"), warn = FALSE)
start <- grep("^```\\{r, broker_cnbc_chunk,", template)
stopifnot(length(start) == 1)
end <- start + which(template[(start + 1):length(template)] == "```")[1]
chunk <- template[start:end]
input <- file.path(out, "broker-section-preview.Rmd")
writeLines(c("---", "title: 'CNBC-TV18 broker-section preview'", "params:", "  ticker: ''",
  "output:", "  html_document:", "    theme: flatly", "    self_contained: true", "---", "",
  "# `r params$ticker`", "",
  "Broker-section preview only. Existing Broker Narrative and Broker Timeline are unchanged and are not recomputed here.",
  "", "```{r, preview_setup, echo=FALSE}", "ticker <- params$ticker", "```", "", chunk), input)

expected <- list(DIVISLAB = c("JPMorgan", "Overweight", "INR 12,500"),
  HINDALCO = c("Citi", "Neutral", "INR 1,050", "Satish Pai"),
  TATASTEEL = c("Goldman Sachs", "Neutral", "INR 215", "Nomura", "INR 210", "INR 240"))
for(ticker in names(expected)) {
  result <- readCnbcStatements(ticker)
  stopifnot(result$status == "ok", nrow(result$data) > 0)
  output <- render(input, output_file = paste0("broker-", ticker, ".html"),
    output_dir = out, params = list(ticker = ticker), knit_root_dir = root,
    envir = new.env(parent = globalenv()), quiet = TRUE)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  stopifnot(all(vapply(expected[[ticker]], grepl, logical(1), x = html, fixed = TRUE)))
  stopifnot(grepl("https://x.com/CNBCTV18Live/status/", html, fixed = TRUE))
  cat(ticker, ":", nrow(result$data), "statements;", sum(result$data$is_recommendation),
    "ratings/targets; rendered", output, "\n")
}
after <- feedState(dbPath)
stopifnot(identical(before, after))
cat("Database checksum, row counts and sidecar inventory unchanged.\n")
print(after)
saveRDS(list(before = before, after = after), file.path(out, "read-only-verification.rds"))
# Parse every real report chunk without running the full generator or touching SQL Server.
purlOutput <- file.path(out, "risk-chunks.R")
knitr::purl(file.path(root, "rp-eq-in-risk.Rmd"), output = purlOutput, quiet = TRUE)
invisible(parse(purlOutput))
cat("All full-report R chunks parse successfully.\n")
