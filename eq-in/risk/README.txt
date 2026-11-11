CNBC-TV18 statements in equity risk reports
==========================================

The equity risk template appends source-attributed ratings, explicit price targets,
and company commentary from the extracted CNBC-TV18 SQLite feed. It leaves the
existing Broker Narrative, Broker Timeline, AI Summary, and return plots unchanged.
It does not run extraction, OCR, SQL Server updates, or performance calculations.

Configuration
-------------

The default database on Siberia is:
  /mnt/icelandD/x-stream/market_statements.sqlite

To use a different database, set STOCKVIZ_X_STREAM_DB in the environment of the R
report process. For example, local validation on Iceland uses:
  STOCKVIZ_X_STREAM_DB=/mnt/data/x-stream/market_statements.sqlite

There is no guessed local-path fallback. An unset variable uses the production
mount. An empty override, missing file, unreadable file, or incompatible schema
produces a visible 'feed unavailable' notice without creating a database. A valid
feed with no company matches displays 'No recorded CNBC-TV18 statements'.

Dependencies: DBI, RSQLite, and htmltools. Regression tests also require testthat.
Preview rendering also requires rmarkdown, knitr, and Pandoc. These packages are
already part of, or supplemental to, the existing report toolchain.

Data contract and presentation
------------------------------

- Read v_market_statements joined to tweets, using a bound exact ticker and
  kind='company'. Include only tweets with status='completed_relevant'.
- Keep all historical matches through the render time. Exclude future publication
  timestamps. Sort each subsection newest first using UTC publication timestamps.
- Deduplicate only the tweet/statement/subject identity. Do not collapse distinct
  statements, conflicting brokers, or repeated calls on different dates.
- Retain raw ratings such as Neutral and Overweight. Show positive finite targets
  only when explicit target evidence exists. Show previous target, target action,
  currency, and horizon only as recorded; do not infer them from CMP or returns.
- Separate ratings/targets from other company commentary. Ownership disclosures
  such as 'own' remain commentary, not Buy recommendations. Preserve each speaker,
  affiliation, and designation; not every speaker is a broker.
- Link the publication time, displayed in IST, to an HTTPS CNBC-TV18 X/Twitter
  status URL only when its ID matches the source tweet. Escape all extracted text.
- Do not insert these statements into the document-derived narrative or timeline,
  invent AUTO_ID values, or calculate predicted/actual returns or outcomes.

SQLite safety
-------------

Connections use SQLITE_RO, synchronous=NULL, a connection-local query_only flag,
closed connections, and a bounded busy timeout. No CREATE, INSERT, UPDATE, DELETE,
journal-mode change, checkpoint, or migration is run against the feed. Missing
files are checked before opening SQLite.

SQLite's WAL reader can open WAL/shared-memory sidecars and perform shared-memory
housekeeping even with SQLITE_RO. The database and WAL content remain read-only;
SQLITE_RO is not a promise of zero filesystem activity in the shared-memory lock
file. The real-data preview checks main database and existing sidecar checksums,
sidecar inventory, and table counts before and after rendering. Concurrent feed
updates can legitimately fail that equality check: rerun against a quiet feed.

The mount must support SQLite's locking and WAL read requirements. If it does not,
the section is visibly unavailable. Never use immutable=1 or copy only the main
file of a live WAL database: either can hide committed statements in the WAL.
Do not alter writer permissions, journal settings, or extractor scheduling to make
a report work.

Regression tests
----------------

From reports02/eq-in/risk:
  Rscript tests/test-broker-x-statements.R .

The tests use disposable local SQLite fixtures, not production writes. They cover
company/status/time filtering, duplicates, original ratings, target evidence,
ownership and management commentary, escaped text, safe source links, IST times,
missing/corrupt schemas, explicit path configuration, and preserved legacy report
content. The legacy-content checksum is a deliberate guard for this append-only
integration; update it only after reviewing intentional legacy report changes.

Real-data broker-section preview
--------------------------------

Choose an output directory outside the published report tree:
  Rscript tests/render-broker-x-preview.R . /PATH/TO/UNPUBLISHED_PREVIEW

On Iceland, prefix the command with the explicit local override:
  STOCKVIZ_X_STREAM_DB=/mnt/data/x-stream/market_statements.sqlite Rscript tests/render-broker-x-preview.R . /PATH/TO/UNPUBLISHED_PREVIEW

The script extracts the exact new chunk from the production template and renders
standalone, self-contained HTML for DIVISLAB, HINDALCO, and TATASTEEL. It verifies
known real broker names and targets, saves read-only-verification.rds, and parses
all R chunks from the full template. Inspect every HTML file in a browser, including
the last row, rightmost commentary, source links, and disclosures. This preview
does not exercise the full report's NORWAY, document database, or production host
configuration, and it does not publish or overwrite existing company reports.

Files
-----

rp-eq-in-risk.Rmd                 Existing template; appended CNBC chunk only.
broker-x-statements.R             Configurable loader and escaped HTML renderer.
tests/test-broker-x-statements.R  Offline regression tests.
tests/render-broker-x-preview.R  Exact-chunk real-data render and read-only checks.
