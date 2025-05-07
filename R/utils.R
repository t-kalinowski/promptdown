#' @import ragnar
#' @import stringi
#' @import ellmer
#' @import S7
#' @import rlang
#' @importFrom dotty .
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


map_lgl <- function(.x, .f, ...) {
  `names<-`(
    vapply(X = .x, FUN = .f, FUN.VALUE = TRUE, ..., USE.NAMES = FALSE),
    value = names(.x)
  )
}

map_chr <- function(.x, .f, ...) {
  `names<-`(
    vapply(X = .x, FUN = .f, FUN.VALUE = "", ..., USE.NAMES = FALSE),
    value = names(.x)
  )
}

`prepend<-` <- function(x, value) {
  c(x[0L], value, x)
}

`append<-` <- function(x, after = NULL, value) {
  if (is.null(after)) c(x, value) else append(x, value, after)
}

`append1<-` <- function(x, value) {
  stopifnot(is.list(x) || identical(mode(x), mode(value)))
  x[[length(x) + 1L]] <- value
  x
}

stri_collapse <- function(
  ...,
  trim_trailing_horizontal_ws = TRUE,
  end_with_newline = NULL,
  sep = ""
) {
  x <- stri_flatten(
    unlist(list(character(), ...), use.names = FALSE),
    sep,
    na_empty = TRUE
  )

  if (trim_trailing_horizontal_ws)
    x <- stri_replace_all_regex(x, '\\h+\n', '\n')

  if (!is.null(end_with_newline)) {
    has_nl <- stri_endswith_fixed(x, "\n")
    if (end_with_newline && !has_nl) {
      x <- stri_c(x, "\n")
    } else if (!end_with_newline && has_nl) {
      x <- stri_trim_right(x)
    }
  }

  x
}

stri_collapse_lines <- function(...) stri_collapse(..., sep = "\n")
stri_flatten_lines <- stri_collapse_lines


is_rstudio <- function() {
  exists("RStudio.Version", envir = globalenv())
}

is_positron <- function() {
  exists(".ps.ark.version", envir = globalenv())
}
