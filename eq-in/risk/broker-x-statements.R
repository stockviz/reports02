# Dependencies are deliberately declared at the top; no extraction or network calls.
# Uses DBI, RSQLite and htmltools via their namespaces.

# Resolve only the production mount or an explicitly configured database path.
cnbcStatementsDbPath <- function() {
  Sys.getenv("STOCKVIZ_X_STREAM_DB", unset = "/mnt/icelandD/x-stream/market_statements.sqlite")
}

# Retain column types even when the feed is empty or unavailable.
emptyCnbcStatements <- function() {
  data.frame(tweet_id = character(), statement_id = integer(), subject_id = integer(),
    source_url = character(), published_at_utc = character(), speaker_name = character(),
    speaker_entity = character(), speaker_designation = character(), what_said = character(),
    evidence = character(), ticker = character(), kind = character(), call = character(),
    call_normalized = character(), price_target = double(), previous_target = double(),
    target_currency = character(), target_action = character(), target_evidence = character(),
    horizon = character(), is_recommendation = logical(), stringsAsFactors = FALSE)
}

# Read completed, exact-company statements without creating or modifying SQLite files.
readCnbcStatements <- function(ticker, dbPath = cnbcStatementsDbPath(), asOf = Sys.time()) {
  result <- list(status = "unavailable", message = "CNBC-TV18 feed unavailable.",
    data = emptyCnbcStatements())
  if(length(ticker) != 1 || is.na(ticker) || !grepl("^[A-Z0-9&._-]+$", ticker)) {
    result$message <- "CNBC-TV18 feed unavailable: invalid ticker."
    return(result)
  }
  if(length(dbPath) != 1 || is.na(dbPath) || !nzchar(dbPath) || !file.exists(dbPath)) {
    result$message <- "CNBC-TV18 feed unavailable: database missing or not configured."
    return(result)
  }
  tryCatch({
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname = dbPath,
      flags = RSQLite::SQLITE_RO, synchronous = NULL)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
    # query_only is connection-local defense in depth, not a database journal setting.
    DBI::dbExecute(con, "PRAGMA query_only = ON")
    data <- DBI::dbGetQuery(con, "
      SELECT v.tweet_id, v.statement_id, v.subject_id, v.source_url, v.published_at_utc,
             v.speaker_name, v.speaker_entity, v.speaker_designation, v.what_said,
             v.evidence, v.ticker, v.kind, v.call, v.call_normalized, v.price_target,
             v.previous_target, v.target_currency, v.target_action, v.target_evidence, v.horizon
      FROM v_market_statements v JOIN tweets t ON t.tweet_id = v.tweet_id
      WHERE v.ticker = $1 AND v.kind = 'company' AND t.status = 'completed_relevant'
        AND julianday(v.published_at_utc) <= julianday($2)
      ORDER BY julianday(v.published_at_utc) DESC, v.statement_id DESC, v.subject_id DESC",
      params = list(ticker, format(asOf, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
    if(nrow(data) == 0) {
      result$status <- "ok"
      result$message <- "No recorded CNBC-TV18 statements for this ticker."
      return(result)
    }
    data <- data[!duplicated(data[c("tweet_id", "statement_id", "subject_id")]), , drop = FALSE]
    target <- suppressWarnings(as.numeric(data$price_target))
    previous <- suppressWarnings(as.numeric(data$previous_target))
    explicit <- !is.na(data$target_evidence) & nzchar(trimws(data$target_evidence))
    valid <- is.finite(target) & target > 0 & explicit
    data$price_target <- ifelse(valid, target, NA_real_)
    data$previous_target <- ifelse(valid & is.finite(previous) & previous > 0, previous, NA_real_)
    rating <- tolower(trimws(ifelse(is.na(data$call), data$call_normalized, data$call)))
    rating <- gsub("[_-]", " ", rating)
    known <- rating %in% c("buy", "sell", "hold", "add", "accumulate", "reduce", "overweight",
      "underweight", "equalweight", "equal weight", "neutral", "outperform", "underperform",
      "top pick", "positive", "negative", "not rated", "unrated", "under review",
      "subscribe", "exit", "dropping coverage")
    data$is_recommendation <- (known | valid) & !(rating %in% c("own", "owned", "ownership"))
    list(status = "ok", message = "", data = data)
  }, error = function(e) {
    # Never leak connection strings or turn a damaged/mismatched schema into 'no calls'.
    result$message <- "CNBC-TV18 feed unavailable: could not read required database schema/data."
    result
  })
}

# Render missing text consistently without interpreting it as HTML.
cnbcText <- function(value, fallback = "\u2014") {
  if(length(value) == 0 || is.na(value) || !nzchar(trimws(value))) fallback else as.character(value)
}

# Format explicit prices without guessing a currency or a forecast horizon.
cnbcPrice <- function(value, currency) {
  if(is.na(value)) return("\u2014")
  paste(cnbcText(currency, "Currency unspecified"),
    format(value, big.mark = ",", scientific = FALSE, trim = TRUE))
}

# Render an original CNBC source link only when both its host/account and ID match.
cnbcPublished <- function(row) {
  utc <- sub("(\\+00:00|Z)$", "", row$published_at_utc)
  time <- as.POSIXct(utc, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")
  label <- format(time, "%Y-%m-%d", tz = "Asia/Kolkata")
  url <- cnbcText(row$source_url, "")
  safe <- grepl("^https://(x\\.com|twitter\\.com)/CNBCTV18Live/status/[0-9]+$", url) &&
    identical(sub(".*/", "", url), as.character(row$tweet_id))
  if(safe) htmltools::tags$a(label, href = url, target = "_blank", rel = "noopener noreferrer") else label
}

# Present each attributed statement with its unchanged rating, targets and commentary.
cnbcStatementRow <- function(row) {
  tags <- htmltools::tags
  attribution <- tags$div(tags$strong(cnbcText(row$speaker_entity,
    cnbcText(row$speaker_name, "Speaker unspecified"))))
  if(!is.na(row$speaker_name) && nzchar(row$speaker_name) &&
     !identical(row$speaker_name, row$speaker_entity)) {
    attribution <- htmltools::tagList(attribution, tags$div(row$speaker_name))
  }
  if(!is.na(row$speaker_designation) && nzchar(row$speaker_designation)) {
    attribution <- htmltools::tagList(attribution, tags$div(row$speaker_designation))
  }
  terms <- tags$div(tags$strong(cnbcText(row$call)))
  if(!is.na(row$price_target)) {
    terms <- htmltools::tagList(terms,
      tags$div(paste("Target:", cnbcPrice(row$price_target, row$target_currency))))
    if(!is.na(row$previous_target)) {
      terms <- htmltools::tagList(terms,
        tags$div(paste("Previous target:", cnbcPrice(row$previous_target, row$target_currency))))
    }
    if(!is.na(row$target_action) && nzchar(row$target_action)) {
      terms <- htmltools::tagList(terms, tags$div(paste("Target action:", row$target_action)))
    }

  }
  if(!is.na(row$horizon) && nzchar(row$horizon)) {
    terms <- htmltools::tagList(terms, tags$div(paste("Horizon:", row$horizon)))
  }
  tags$tr(tags$td(cnbcPublished(row)), tags$td(attribution), tags$td(terms),
    tags$td(cnbcText(row$what_said)))
}

# Build a static, escaped, responsive section; keep non-rating commentary separate.
cnbcStatementsHtml <- function(result) {
  tags <- htmltools::tags
  heading <- tags$h2("CNBC-TV18: ratings, targets and commentary")
  if(result$status != "ok" || nrow(result$data) == 0) {
    return(tags$section(class = "cnbc-statements", heading,
      tags$p(role = if(result$status == "ok") "status" else "alert", result$message)))
  }
  data <- result$data
  content <- list()
  for(recommendations in c(TRUE, FALSE)) {
    part <- data[data$is_recommendation == recommendations, , drop = FALSE]
    if(nrow(part) == 0) next
    rows <- lapply(seq_len(nrow(part)), function(i) cnbcStatementRow(part[i, , drop = FALSE]))
    content <- c(content, list(
      tags$h3(if(recommendations) "Ratings and targets" else "Other company commentary (not broker ratings)"),
      tags$div(class = "cnbc-table-scroll", tags$table(class = "cnbc-table",
        tags$thead(tags$tr(tags$th("Published / source (IST)"), tags$th("Speaker / affiliation"),
          tags$th("Rating / target"), tags$th("Commentary"))), tags$tbody(rows)))))
  }
  tags$section(class = "cnbc-statements", heading,
    tags$style(".cnbc-table-scroll{overflow-x:auto;margin-bottom:20px}.cnbc-table{width:100%;min-width:850px;border-collapse:collapse;font-size:14px}.cnbc-table th,.cnbc-table td{padding:10px;vertical-align:top;border:1px solid #ddd;overflow-wrap:anywhere}.cnbc-table th{background:#eef3f6}.cnbc-table th:nth-child(1){width:16%}.cnbc-table th:nth-child(2){width:17%}.cnbc-table th:nth-child(3){width:24%}.cnbc-table small{display:block;margin-top:8px;color:#555}"),
    tags$p("Extracted from CNBC-TV18 posts; ratings and targets are as published, not current consensus. OCR may contain errors. Speaker attribution is retained; not all speakers are brokers. These statements are separate from the Broker Narrative and Broker Timeline and have no calculated performance or outcomes."),
    content)
}
