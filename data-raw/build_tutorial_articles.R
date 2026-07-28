# Generates static, non-interactive pkgdown articles from the learnr
# tutorials in inst/tutorials/. Run this after editing a tutorial's .Rmd,
# and commit the regenerated files under vignettes/articles/.
#
# These articles are deliberately a *preview*, not a substitute for the
# interactive tutorial: code output and exercise solutions are withheld so
# that readers are motivated to install the package and run `run_tute()`.
#
# What the transform does to each source tutorial:
# - drops `runtime: shiny_prerendered` and switches to a plain html_document
# - drops `library(learnr)`, and sets `echo = TRUE, eval = FALSE` so the
#   demonstration code is shown but not run (no output is displayed).
#   Inline `r gloss()`/`r print_glossary()` calls still evaluate, since
#   `eval = FALSE` only applies to fenced chunks, so the glossary and the
#   hover-over term definitions still work.
# - for `exercise = TRUE` chunks with a real body (demonstrations), keeps
#   the code as an ordinary (non-evaluated) chunk
# - for blank exercise chunks (the reader was meant to type the answer),
#   drops the chunk entirely — the solution is never shown
# - drops `<label>-hint*`, `<label>-solution`, and `<label>-setup` scaffold
#   chunks (learnr-only)
# - replaces `question()`/`quiz()` chunks with a static callout pointing
#   back to the interactive tutorial (`run_tute()`)

chunk_start_re <- "^```\\{r[ ]*([A-Za-z0-9_.-]*)[ ]*(,\\s*(.*))?\\}\\s*$"

parse_rmd <- function(lines) {
  chunks <- list()
  segments <- list()
  i <- 1
  n <- length(lines)
  while (i <= n) {
    m <- regmatches(lines[i], regexec(chunk_start_re, lines[i]))[[1]]
    if (length(m) > 0) {
      label <- m[2]
      opts <- if (length(m) >= 4) m[4] else ""
      j <- i + 1
      while (j <= n && trimws(lines[j]) != "```") j <- j + 1
      body <- if (j > i + 1) lines[(i + 1):(j - 1)] else character(0)
      chunks[[length(chunks) + 1]] <- list(label = label, opts = opts, body = body)
      segments[[length(segments) + 1]] <- list(type = "chunk", idx = length(chunks))
      i <- j + 1
    } else {
      start <- i
      while (i <= n) {
        m2 <- regmatches(lines[i], regexec(chunk_start_re, lines[i]))[[1]]
        if (length(m2) > 0) break
        i <- i + 1
      }
      segments[[length(segments) + 1]] <- list(type = "prose", lines = lines[start:(i - 1)])
    }
  }
  list(chunks = chunks, segments = segments)
}

strip_exercise_opts <- function(opts) {
  parts <- if (nzchar(opts)) strsplit(opts, ",\\s*")[[1]] else character(0)
  parts <- parts[!grepl("^exercise(\\.setup)?\\s*=", parts)]
  paste(parts, collapse = ", ")
}

quiz_callout <- paste0(
  '::: {.callout}\n',
  '<span class="callout-label">**Try it yourself**:</span> ',
  "This section includes an interactive quiz in the live tutorial — ",
  "run `run_tute()` at the R console to try it.\n",
  ':::'
)

build_article <- function(src_path, out_path) {
  lines <- readLines(src_path, warn = FALSE)
  parsed <- parse_rmd(lines)
  chunks <- parsed$chunks
  labels <- vapply(chunks, function(x) x$label, character(1))

  is_quiz <- vapply(chunks, function(x) {
    grepl("\\b(question|quiz)\\s*\\(", paste(x$body, collapse = "\n"))
  }, logical(1))

  base_of_hint <- sub("-hint(-[0-9]+)?$", "", labels)
  is_hint <- grepl("-hint(-[0-9]+)?$", labels) & (base_of_hint %in% labels) & !is_quiz

  base_of_solution <- sub("-solution$", "", labels)
  is_solution_scaffold <- grepl("-solution$", labels) & (base_of_solution %in% labels) & !is_quiz

  is_setup_scaffold <- labels != "setup" & grepl("-setup$", labels) & !is_quiz

  is_scaffold <- is_hint | is_solution_scaffold | is_setup_scaffold

  # keep demonstration chunks (a real body); drop blank exercises entirely
  # so their solutions are never revealed in the static preview
  resolved_body <- vector("list", length(chunks))
  drop_primary <- logical(length(chunks))
  for (k in seq_along(chunks)) {
    if (is_quiz[k] || is_scaffold[k] || labels[k] == "setup") next
    body <- chunks[[k]]$body
    if (all(trimws(body) == "")) {
      drop_primary[k] <- TRUE
    } else {
      resolved_body[[k]] <- body
    }
  }

  render_chunk <- function(k) {
    if (is_scaffold[k]) return(NULL)
    label <- chunks[[k]]$label
    if (label == "setup") {
      body <- chunks[[k]]$body
      body <- body[!grepl("^\\s*library\\(learnr\\)\\s*$", body)]
      body <- sub("echo\\s*=\\s*FALSE", "echo = TRUE, eval = FALSE", body)
      return(c(paste0("```{r ", label,
                       if (nzchar(chunks[[k]]$opts)) paste0(", ", chunks[[k]]$opts) else "",
                       "}"), body, "```"))
    }
    if (is_quiz[k]) return(strsplit(quiz_callout, "\n")[[1]])
    if (drop_primary[k]) return(NULL)
    opts <- strip_exercise_opts(chunks[[k]]$opts)
    header <- paste0("```{r ", label, if (nzchar(opts)) paste0(", ", opts) else "", "}")
    c(header, resolved_body[[k]], "```")
  }

  out <- character(0)
  last_was_quiz <- FALSE
  for (seg in parsed$segments) {
    if (seg$type == "prose") {
      if (last_was_quiz && all(trimws(seg$lines) == "")) next  # blank gap between quizzes
      out <- c(out, seg$lines)
      last_was_quiz <- FALSE
    } else {
      k <- seg$idx
      if (is_quiz[k] && last_was_quiz) next  # collapse consecutive quiz callouts
      rendered <- render_chunk(k)
      if (!is.null(rendered)) out <- c(out, rendered)
      last_was_quiz <- is_quiz[k]
    }
  }

  text <- paste(out, collapse = "\n")
  text <- gsub("\n{3,}", "\n\n\n", text)  # tidy up gaps left by dropped scaffold chunks

  # YAML: drop the learnr/shiny_prerendered output, use a plain html_document
  text <- sub(
    "output:\\n  learnr::tutorial:\\n    theme: flatly\\nruntime: shiny_prerendered",
    "output: rmarkdown::html_document",
    text
  )

  # banner pointing back to the interactive version, right before the
  # first section heading
  title <- sub('.*title:\\s*"([^"]*)".*', "\\1", grep("^title:", lines, value = TRUE)[1])
  banner <- paste0(
    '::: {.callout}\n',
    '<span class="callout-label">**Static preview**:</span> ',
    "This is a static, read-only preview of the \"", title, "\" tutorial. ",
    "To keep it a preview, the code output and exercise solutions are not ",
    "shown here, and the quizzes are replaced with notes like this one. ",
    "Install the package and run `run_tute()` at the R console to work ",
    "through the tutorial interactively — running the code, seeing the ",
    "results, and getting hints, solutions, and quizzes.\n",
    ':::'
  )
  text <- sub("\\n(## )", paste0("\n\n", banner, "\n\n\\1"), text)

  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  writeLines(text, out_path)
  invisible(text)
}

build_article(
  "inst/tutorials/manynet1/making.Rmd",
  "vignettes/articles/making-network-data.Rmd"
)
build_article(
  "inst/tutorials/manynet2/manipulating.Rmd",
  "vignettes/articles/manipulating-network-data.Rmd"
)

# No data/ folder is copied: with `eval = FALSE` the `read_edgelist("data/...")`
# calls are shown but never run, so the data files are not needed to render.
