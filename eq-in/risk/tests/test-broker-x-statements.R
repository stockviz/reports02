library(DBI)
library(RSQLite)
library(testthat)
library(htmltools)

args <- commandArgs(trailingOnly = TRUE)
root <- if(length(args)) args[1] else "."
helper <- file.path(root, "broker-x-statements.R")
test_that("CNBC integration helper exists", {
  expect_true(file.exists(helper))
})
source(helper)

# Build a disposable extraction-schema fixture, never the production database.
makeFixture <- function() {
  path <- tempfile(fileext = ".sqlite")
  con <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))
  rows <- data.frame(
    tweet_id = as.character(101:111), statement_id = 1:11, subject_id = 1:11,
    source_url = paste0("https://x.com/CNBCTV18Live/status/", 101:111),
    published_at_utc = rep("2026-11-09T02:01:38+00:00", 11),
    speaker_name = NA_character_, speaker_entity = "Citi", speaker_designation = NA_character_,
    what_said = "A <script>alert('x')</script> & quoted view", evidence = "card text",
    ticker = c(rep("HINDALCO", 9), "DIVISLAB", "HINDALCO"),
    kind = c(rep("company", 8), "industry", "company", "company"),
    call = c("Neutral", "Overweight", "own", NA, "Buy", "Buy", "Buy", "Hold", "Buy", "Buy", NA),
    call_normalized = c("neutral", "overweight", "own", NA, "buy", "buy", "buy", "hold", "buy", "buy", NA),
    price_target = c(1050, 12500, NA, NA, 999, NA, NA, NA, NA, NA, 123),
    previous_target = c(1100, NA, rep(NA_real_, 9)), target_currency = "INR",
    target_action = c("lowered", rep(NA_character_, 10)),
    target_evidence = c("Target cut to 1050", "Target 12500", rep(NA_character_, 8), ""),
    horizon = c("12 months", rep(NA_character_, 10)), stringsAsFactors = FALSE
  )
  rows$source_url[8] <- "javascript:alert(1)"
  rows$published_at_utc[7] <- "2099-01-01T00:00:00Z"
  rows$speaker_name[4] <- "Manager"
  rows$speaker_entity[4] <- "Hindalco"
  rows$speaker_designation[4] <- "Managing director"
  tweets <- data.frame(tweet_id = rows$tweet_id, status = "completed_relevant")
  tweets$status[5] <- "failed"
  dbWriteTable(con, "tweets", tweets)
  dbWriteTable(con, "flat_fixture", rbind(rows, rows[1, ]))
  dbExecute(con, "CREATE VIEW v_market_statements AS SELECT * FROM flat_fixture")
  path
}

fixture <- makeFixture()
asOf <- as.POSIXct("2026-11-11 12:00:00", tz = "UTC")
before <- tools::md5sum(fixture)
result <- readCnbcStatements("HINDALCO", fixture, asOf)

test_that("exact company matching excludes failed, sector and future records", {
  expect_identical(result$status, "ok")
  expect_setequal(result$data$tweet_id, c("101", "102", "103", "104", "106", "108", "111"))
  expect_equal(nrow(result$data), 7)
  expect_true(all(result$data$ticker == "HINDALCO"))
  expect_false(anyDuplicated(result$data[c("tweet_id", "statement_id", "subject_id")]) > 0)
})

test_that("ratings and explicit targets retain their meaning", {
  expect_equal(result$data$call[result$data$tweet_id == "101"], "Neutral")
  expect_equal(result$data$call[result$data$tweet_id == "102"], "Overweight")
  expect_equal(result$data$previous_target[result$data$tweet_id == "101"], 1100)
  expect_false(result$data$is_recommendation[result$data$tweet_id == "103"])
  expect_false(result$data$is_recommendation[result$data$tweet_id == "104"])
  expect_false(result$data$is_recommendation[result$data$tweet_id == "111"])
  expect_true(is.na(result$data$price_target[result$data$tweet_id == "111"]))
})

test_that("rendering escapes all source text and only links original CNBC posts", {
  html <- as.character(cnbcStatementsHtml(result))
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_false(grepl("javascript:", html, fixed = TRUE))
  expect_match(html, "https://x.com/CNBCTV18Live/status/101", fixed = TRUE)
  expect_match(html, ">2026-11-09</a>", fixed = TRUE)
  expect_false(grepl("07:31", html, fixed = TRUE))
  expect_false(grepl("Target evidence:", html, fixed = TRUE))
  expect_match(html, "INR 1,050", fixed = TRUE)
  expect_match(html, "Previous target: INR 1,100", fixed = TRUE)
  expect_match(html, "Managing director", fixed = TRUE)
  expect_match(html, "Other company commentary", fixed = TRUE)
  expect_match(html, "not current consensus", fixed = TRUE)
})

test_that("loading is read-only", {
  expect_identical(unname(tools::md5sum(fixture)), unname(before))
  expect_false(file.exists(paste0(fixture, "-wal")))
  expect_false(file.exists(paste0(fixture, "-shm")))
})

test_that("default is the explicit Siberia mount and overrides are opt-in", {
  old <- Sys.getenv("STOCKVIZ_X_STREAM_DB", unset = NA_character_)
  Sys.unsetenv("STOCKVIZ_X_STREAM_DB")
  expect_identical(cnbcStatementsDbPath(), "/mnt/icelandD/x-stream/market_statements.sqlite")
  Sys.setenv(STOCKVIZ_X_STREAM_DB = fixture)
  expect_identical(cnbcStatementsDbPath(), fixture)
  if(is.na(old)) Sys.unsetenv("STOCKVIZ_X_STREAM_DB") else Sys.setenv(STOCKVIZ_X_STREAM_DB = old)
})

test_that("empty data differs visibly from unavailable data and cannot create a DB", {
  missing <- tempfile(fileext = ".sqlite")
  unavailable <- readCnbcStatements("HINDALCO", missing, asOf)
  expect_identical(unavailable$status, "unavailable")
  expect_false(file.exists(missing))
  expect_match(as.character(cnbcStatementsHtml(unavailable)), "unavailable")
  empty <- readCnbcStatements("UNKNOWN", fixture, asOf)
  expect_identical(empty$status, "ok")
  expect_equal(nrow(empty$data), 0)
  expect_type(empty$data$price_target, "double")
  expect_match(as.character(cnbcStatementsHtml(empty)), "No recorded")
  expect_identical(readCnbcStatements("' OR 1=1 --", fixture, asOf)$status, "unavailable")
  corrupt <- tempfile(fileext = ".sqlite")
  writeLines("not sqlite", corrupt)
  expect_warning(readCnbcStatements("HINDALCO", corrupt, asOf), NA)
  expect_identical(readCnbcStatements("HINDALCO", corrupt, asOf)$status, "unavailable")
  noView <- tempfile(fileext = ".sqlite")
  con <- dbConnect(SQLite(), noView)
  dbWriteTable(con, "unrelated", data.frame(x = 1))
  dbDisconnect(con)
  expect_identical(readCnbcStatements("HINDALCO", noView, asOf)$status, "unavailable")
  unlink(c(corrupt, noView))
})

test_that("the real report integrates CNBC without changing legacy report content", {
  template <- readLines(file.path(root, "rp-eq-in-risk.Rmd"), warn = FALSE)
  expect_true(any(grepl("broker_cnbc_chunk", template, fixed = TRUE)))
  expect_true(any(grepl('source("broker-x-statements.R", local = TRUE)', template, fixed = TRUE)))
  expect_true(any(grepl("cnbcStatementsHtml(readCnbcStatements(ticker))", template, fixed = TRUE)))
  legacy <- tempfile()
  writeLines(template[seq_len(388)], legacy, useBytes = TRUE)
  expect_identical(unname(tools::md5sum(legacy)), "8882ea162fdc4cc7a873570118c58c6b")
  unlink(legacy)
})

unlink(fixture)
cat("CNBC regression tests complete.\n")
