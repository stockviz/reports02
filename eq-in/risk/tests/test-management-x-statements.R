library(testthat)
library(htmltools)
args <- commandArgs(trailingOnly = TRUE)
root <- if(length(args)) args[1] else "."
source(file.path(root, "broker-x-statements.R"))

# Exercise classification without inventing titles for title-less real feed rows.
test_that("management requires explicit role and affiliation, never investor inference", {
  rows <- data.frame(speaker_designation = c("CEO & MD", "management", "Managing director", NA,
    "CEO", "Research director", "Investment director", "Fund manager", "CFO", "CEO"),
    speaker_entity = c("LIC", "Bosch", "Hindalco", "Hindalco", "Envision Capital",
      "Broker Research", "Capital Partners", "Mutual Fund", NA, "Kotak Mahindra AMC"))
  expect_true(exists("cnbcIsManagement", mode = "function"))
  if(exists("cnbcIsManagement", mode = "function"))
    expect_identical(cnbcIsManagement(rows), c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))
})

# Use the existing real SQL fixture to retain no-peeking and read-only guarantees.
source(file.path(root, "tests/test-broker-x-statements.R"))
fixture <- makeFixture()
result <- readCnbcStatements("HINDALCO", fixture, asOf)
test_that("loaded management flag shares exact-company/status/asOf filtering", {
  expect_true("is_management" %in% names(result$data))
  if("is_management" %in% names(result$data))
    expect_identical(result$data$tweet_id[result$data$is_management], "104")
  expect_true("is_management" %in% names(emptyCnbcStatements()))
})

test_that("distinct section escapes quotes, links IST dates and removes duplicate commentary", {
  expect_true(exists("cnbcManagementHtml", mode = "function"))
  if(!exists("cnbcManagementHtml", mode = "function")) return(invisible(NULL))
  html <- as.character(cnbcManagementHtml(result))
  expect_match(html, "Management comments", fixed = TRUE)
  expect_match(html, "Managing director", fixed = TRUE)
  expect_match(html, "Hindalco", fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_false(grepl("Rating / target", html, fixed = TRUE))
  expect_match(html, ">2026-11-09</a>", fixed = TRUE)
  expect_false(grepl("Target evidence", html, fixed = TRUE))
  expect_false(grepl("Managing director", as.character(cnbcStatementsHtml(result)), fixed = TRUE))
  # Off-issuer management must retain its employer; never claim it belongs to the subject.
  other <- result
  other$data$speaker_entity[other$data$is_management] <- "LIC"
  html <- as.character(cnbcManagementHtml(other))
  expect_match(html, "LIC", fixed = TRUE)
  expect_match(html, "not necessarily management of this company", fixed = TRUE)
})

test_that("management empty and unavailable states remain distinct and missing DB is not created", {
  if(!exists("cnbcManagementHtml", mode = "function")) skip("renderer not implemented")
  empty <- readCnbcStatements("UNKNOWN", fixture, asOf)
  expect_match(as.character(cnbcManagementHtml(empty)), "No recorded management comments", fixed = TRUE)
  missing <- tempfile(fileext = ".sqlite")
  expect_match(as.character(cnbcManagementHtml(readCnbcStatements("HINDALCO", missing, asOf))), "unavailable", fixed = TRUE)
  expect_false(file.exists(missing))
  row <- result$data[result$data$is_management, , drop = FALSE]
  row$published_at_utc <- "2026-11-09T23:30:00Z"
  row$source_url <- "javascript:alert(1)"
  html <- as.character(cnbcManagementHtml(list(status = "ok", data = row)))
  expect_match(html, "2026-11-10", fixed = TRUE)
  expect_false(grepl("javascript:", html, fixed = TRUE))
})

# Group by the local publication day and named person, retaining every source.
test_that("same-person same-IST-day management comments share one row", {
  row <- result$data[result$data$is_management, , drop = FALSE]
  rows <- row[rep(1, 4), , drop = FALSE]
  rows$speaker_name <- c("Example Person", " Example Person ", "Example Person", "Other Person")
  rows$published_at_utc <- c("2026-11-09T20:00:00Z", "2026-11-10T01:00:00Z",
    "2026-11-10T20:00:00Z", "2026-11-10T01:00:00Z")
  rows$tweet_id <- c("201", "202", "203", "204")
  rows$source_url <- paste0("https://x.com/CNBCTV18Live/status/", rows$tweet_id)
  rows$what_said <- c("First <comment>", "Second comment", "Next day comment", "Other person's comment")
  html <- as.character(cnbcManagementHtml(list(status = "ok", data = rows)))
  expect_equal(length(gregexpr("<tr>", html, fixed = TRUE)[[1]]), 4L) # Header + three groups.
  expect_equal(length(gregexpr("<li>", html, fixed = TRUE)[[1]]), 4L)
  expect_match(html, "First &lt;comment&gt;", fixed = TRUE)
  expect_match(html, "Second comment", fixed = TRUE)
  for(id in rows$tweet_id) expect_match(html, paste0("/status/", id, '"'), fixed = TRUE)
  expect_match(html, "2026-11-11", fixed = TRUE)
  expect_false(grepl("20:00", html, fixed = TRUE))
  # Never merge unnamed speakers from different affiliations.
  rows <- rows[1:2, ]
  rows$speaker_name <- NA_character_
  rows$speaker_entity <- c("Company A", "Company B")
  html <- as.character(cnbcManagementHtml(list(status = "ok", data = rows)))
  expect_equal(length(gregexpr("<tr>", html, fixed = TRUE)[[1]]), 3L)
  rows$speaker_entity <- "Same Company"
  html <- as.character(cnbcManagementHtml(list(status = "ok", data = rows)))
  expect_equal(length(gregexpr("<tr>", html, fixed = TRUE)[[1]]), 3L)
})

unlink(fixture)
cat("Management regression tests complete.\n")
