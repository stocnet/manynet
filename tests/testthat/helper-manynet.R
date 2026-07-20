options(manynet_verbosity = "quiet")
options(snet_verbosity = "quiet")

expect_values <- function(object, ref, toler = 3) {
  # 1. Capture object and label
  # act <- quasi_label(rlang::enquo(object), arg = "object")
  act <- list(val = object, label = deparse(substitute(object)))

  # 2. Call expect()
  act$n <- round(c(unname(unlist(act$val))), toler)
  ref <- round(c(unname(unlist(ref))), toler)
  expect(
    act$n == ref,
    sprintf("%s has values %f, not values %f.", act$lab, act$n, ref)
  )

  # 3. Invisibly return the value
  invisible(act$val)
}

expect_mark <- function(object, ref, top = 3) {
  # 1. Capture object and label
  # act <- quasi_label(rlang::enquo(object), arg = "object")
  act <- list(val = object, label = deparse(substitute(object)))

  # 2. Call expect()
  act$n <- as.character(c(unname(unlist(act$val)))[1:top])
  ref <- as.character(c(unname(unlist(ref)))[1:top])
  expect(
    all(act$n == ref),
    sprintf(
      "%s has values %s, not values %s.",
      act$lab,
      paste(act$n, collapse = ", "),
      paste(ref, collapse = ", ")
    )
  )

  # 3. Invisibly return the value
  invisible(act$val)
}


top3 <- function(res, dec = 4) {
  if (is.numeric(res)) {
    unname(round(res, dec))[1:3]
  } else {
    unname(res)[1:3]
  }
}

bot3 <- function(res, dec = 4) {
  lr <- length(res)
  if (is.numeric(res)) {
    unname(round(res, dec))[(lr - 2):lr]
  } else {
    unname(res)[(lr - 2):lr]
  }
}

top5 <- function(res, dec = 4) {
  if (is.numeric(res)) {
    unname(round(res, dec))[1:5]
  } else {
    unname(res)[1:3]
  }
}

bot5 <- function(res, dec = 4) {
  lr <- length(res)
  if (is.numeric(res)) {
    unname(round(res, dec))[(lr - 4):lr]
  } else {
    unname(res)[(lr - 2):lr]
  }
}

funs_objs <- mget(ls("package:manynet"), inherits = TRUE)

set.seed(1234)
data_objs <- list(
  directed = generate_random(12, directed = TRUE),
  twomode = generate_random(c(6, 6)),
  labelled = to_signed(add_node_attribute(
    create_wheel(12),
    "name",
    LETTERS[1:12]
  )),
  attribute = add_node_attribute(
    create_ring(12),
    "group",
    rep(c("A", "B"), each = 6)
  ),
  weighted = add_tie_attribute(
    create_ring(12),
    "weight",
    rep(c(1, 2), each = 6)
  ),
  diffusion = play_diffusion(
    create_ring(12),
    seeds = 1,
    steps = 5,
    latency = 0.75,
    recovery = 0.25
  )
)

# stocnet object
test_stocnet_obj <- make_stocnet(
  info = list(name = "Test Dataset", directed = TRUE),
  nodes = tibble::tibble(
    id = 1:12,
    group = rep(c("A", "B"), each = 6)
  ),
  ties = tibble::tibble(
    from = 1:12,
    to = c(2:12, 1),
    weight = rep(c(1, 2), each = 6)
  ),
  changes = tibble::tibble(
    node = 1:12,
    time = rep(1:6, each = 2),
    var = "group",
    value = rep(c("A", "B"), each = 6)
  )
)

find_pkg_tutorial_paths <- function(pkg) {
  tute_folders <- list.dirs(system.file("tutorials", package = pkg),
                             recursive = F)
  tute_files <- unlist(lapply(tute_folders, function(folder) {
    list.files(folder, pattern = "*.Rmd", full.names = TRUE)
  }))
  tute_files
}

check_tute_rendering <- function(path, quiet = TRUE){

  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("shiny")
  # Rendering shiny_prerendered learnr tutorials against a covr-instrumented
  # namespace is fragile and adds no coverage value (check_tute_functions()
  # exercises the tutorial code itself); skip under covr to avoid a
  # coverage-only failure on CI.
  skip_if(as.logical(Sys.getenv("R_COVR", "false")),
          "tutorial rendering not tested under covr")
  stopifnot(all(file.exists(path)))
  
  for(i in path){
    if(!quiet) message("Rendering: ", basename(i))
    tryCatch({
      rmarkdown::render(input = i, 
                        output_dir = tempdir(),
                        intermediates_dir = tempdir(), quiet = quiet)
      # Note that the Debian setup on CRAN does not allow for writing files to any
      # location other than the temporary directory, which is why we must specify
      # tempdir() in the two dir arguments.
      if(!quiet) message("Successfully rendered: ", basename(i))
    }, error = function(e) {
      stop("Failed to render ", i, ": ", e$message, call. = FALSE)
    })
  }
  invisible(NULL)
}

check_tute_functions <- function(path, skip = "ergm\\(", quiet = TRUE){
  tmp <- tempfile(fileext = ".R")
  knitr::purl(
    input  = path,
    output = tmp,
    quiet  = quiet
  )
  exprs <- parse(tmp)  # your purled file
  env <- new.env(parent = globalenv())
  
  is_skipped_call <- function(expr) {
    any(grepl(skip, deparse(expr)))
  }

  for (i in seq_along(exprs)) {
    # Stop at the first slow call: it and any later (dependent) expressions
    # are skipped, but we return normally so the caller's loop over the
    # remaining tutorials continues. Using skip() here would unwind to the
    # enclosing test_that() and abort every subsequent tutorial too.
    if (is_skipped_call(exprs[[i]])) {
      break
    }

    w <- NULL
    e <- NULL
    m <- NULL
    
    not_out <- withCallingHandlers(
      tryCatch(
        eval(exprs[[i]], envir = env),
        error = function(err) {
          e <<- err
          NULL
        }
      ),
      warning = function(wrn) {
        w <<- wrn
        invokeRestart("muffleWarning")
      },
      message = function(msg) {
        m <<- c(m, conditionMessage(msg))
        invokeRestart("muffleMessage")
      }
    )
    
    # If there *was* a warning, check if it's a deprecated/defunct one
    if (!is.null(w)) {
      msg <- conditionMessage(w)
      
      # Only fail if it's a deprecated/defunct warning
      if (!grepl("deprecate|defunct|moved", msg, ignore.case = TRUE)) {
        w <- NULL
      }
    }
    
    # Now test what happened
    expect_null(
      e,
      info = paste0("Error in expression ", i,
                    " of ", basename(path), ": ", deparse(exprs[[i]]))
    )
    
    expect_null(
      w,
      info = paste("Warning in expression", i, ":", deparse(exprs[[i]]))
    )
  }
}
