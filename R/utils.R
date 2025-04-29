#' @import ragnar
#' @import stringi
#' @import ellmer
#' @import S7
#' @importFrom dplyr if_else mutate group_by summarise arrange
NULL

last <- function(x) x[[length(x)]]
drop_last <- function(x) x[-length(x)]
drop_first <- function(x) x[-1L]
drop_nulls <- function(x) x[!vapply(x, is.null, FALSE, USE.NAMES = FALSE)]


replace_val <- function(x, old, new) {
  if (!is_scalar(new))
    stop(
      "Unexpected length of replacement value in replace_val().\n",
      "`new` must be length 1, not ",
      length(new)
    )
  x[x %in% old] <-
    new
  x
}

seq_along0 <- function(x) 0L:(length(x) - 1L)

is_scalar <- function(x) identical(length(x), 1L)

imap <- function(.x, .f, ...) {
  out <- .mapply(.f, list(.x, names(.x) %||% seq_along(.x)), list(...))
  names(out) <- names(.x)
  out
}

stri_flatten_lines <- function(..., trim = TRUE, end_with_newline = NULL) {
  x <- stri_flatten(
    unlist(list(character(), ...), use.names = FALSE),
    "\n",
    na_empty = TRUE
  )

  if (trim) {
    x <- stri_replace_all_regex(x, '\\h+\n', '\n')
  }

  if (!is.null(end_with_newline)) {
    has_nl <- stri_endswith_fixed(x, "\n")
    if (end_with_newline && !has_nl) {
      x <- stri_c(x, "\n")
    } else if (!end_with_newline && has_nl) {
      x <- stri_replace_last_regex(x, "\\s*\n+$", "")
    }
  }

  x
}

`append<-` <- function(x, after = NULL, value) {
  if (is.null(after)) c(x, value) else append(x, value, after)
}

`prepend<-` <- function(x, value) {
  c(x[0], value, x)
}


stri_collapse <- function(..., trim = TRUE, end_with_newline = NULL) {
  x <- stri_flatten(
    unlist(list(character(), ...), use.names = FALSE),
    "",
    na_empty = TRUE
  )

  if (trim) {
    x <- stri_replace_all_regex(x, '\\h+\n', '\n')
  }

  if (!is.null(end_with_newline)) {
    has_nl <- stri_endswith_fixed(x, "\n")
    if (end_with_newline && !has_nl) {
      x <- stri_c(x, "\n")
    } else if (!end_with_newline && has_nl) {
      x <- stri_replace_last_regex(x, "\\s*\n+$", "")
    }
  }

  x
}

timestamp_filename <-
  function(prefix = "", ext = NULL, format = "%Y-%m-%d_%Hh%Mm%OS3s") {
    ts <- sub(".", "-", format(Sys.time(), format = format), fixed = TRUE)
    if (!is.null(ext)) ext <- sub("\\.?(.*)$", ".\\1", ext, perl = TRUE)
    paste0(prefix, ts, ext)
  }
