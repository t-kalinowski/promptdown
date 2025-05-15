Emitter <- R6::R6Class(
  "Emitter",
  public = list(
    target = NULL,

    connection = NULL,
    opened_connection = FALSE,
    rstudio_document_id = NULL,
    positron_active_document = FALSE,

    initialize = function(target = NULL) {
      self$target <- target

      if (identical(target, "")) {
        self$connection <- stdout()
      } else if (is.character(target)) {
        if (is_rstudio()) {
          context <- rstudioapi::getSourceEditorContext()
          path <- normalizePath(context$path, mustWork = FALSE)

          if (normalizePath(target, mustWork = FALSE) == path) {
            self$rstudio_document_id <- context$id
            return()
          }
        }

        if (is_positron()) {
          # id not supported yet
          path <- rstudioapi::getSourceEditorContext()$path
          path <- normalizePath(path, mustWork = FALSE)
          if (normalizePath(target, mustWork = FALSE) == path) {
            self$positron_active_document <- TRUE
            return()
          }
        }

        self$connection <- file(target, "a")
        self$opened_connection <- TRUE
      } else if (inherits(target, "connection")) {
        self$connection <- target
      } else if (is.function(target)) {
      } else {
        stop("target must be a file path, connection, or function")
      }
    },

    emit = function(msg) {
      if (!is.null(self$connection)) {
        cat(msg, file = self$connection, sep = "", append = TRUE)
      } else if (
        !is.null(self$rstudio_document_id) || self$positron_active_document
      ) {
        rstudioapi::insertText(
          location = Inf,
          msg,
          id = self$rstudio_document_id
        )
      } else if (is.function(self$target)) {
        self$target(msg)
      }
    },

    close = function() {
      if (
        self$opened_connection &&
          inherits(self$connection, "connection") &&
          isOpen(self$connection)
      ) {
        base::close(self$connection)
        self$connection <- NULL
      }
      if (!is.null(self$rstudio_document_id)) {
        rstudioapi::documentSave(id = self$rstudio_document_id)
      } else if (self$positron_active_document) {
        rstudioapi::documentSave()
      }
    }
  ),

  private = list(
    finalize = function() {
      self$close()
    }
  ),
  class = FALSE
)

MultiEmitter <- R6::R6Class(
  "MultiEmitter",
  public = list(
    emitters = NULL,

    initialize = function(targets) {
      self$emitters <- targets |> drop_nulls() |> lapply(Emitter$new)
    },

    emit = function(...) {
      msg <- stri_flatten(unlist(c(character(), ...)))
      for (emitter in self$emitters) {
        emitter$emit(msg)
      }
    },

    close = function() {
      for (emitter in self$emitters) {
        emitter$close()
      }
    }
  ),
  class = FALSE
)

#' Create a MultiEmitter
#'
#' Constructs a `MultiEmitter` object that wraps multiple `Emitter` instances.
#' Each emitter sends messages to a specified target, which can be a file path,
#' connection, function, or RStudio document.
#'
#' @param ... Individual target destinations. These can include:
#'
#' - A file path as a string (e.g., `"log.txt"`).
#'   If running in RStudio/Positron and the path matches the currently focused
#'   or last-focused document (as returned by
#'   `rstudioapi::documentId(allowConsole = FALSE) |>
#'   rstudioapi::documentPath()`), the message will be inserted using
#'   `rstudioapi::insertText()`. Otherwise, it will be appended to the file
#'   using `cat(sep="")` to a persistant connection to the file opened when the
#'   emitter was constructed. The connection will automatically close when the
#'   object is finalized or when `emitter$close()` is called.
#' - A connection object (e.g., `stdout()` or a file connection).
#' - A function that accepts a character string (e.g., `function(msg) cat(msg)`).
#' - `NULL` (ignored).
#'
#' @param targets A list of target destinations. Defaults to the list of `...`.
#'
#' @return A `MultiEmitter` object that can emit messages to multiple
#'   destinations.
#' @noRd
#' @keywords internal
#' @examples
#' # Emit to console
#' con <- stdout()
#' filepath <- tempfile()
#' fun <- function(msg) cat("fun message:", msg, file = stderr())
#' em <- new_emitter(con, filepath, fun)
#' em$emit("Hello, world!\n")
#' em$close()
#' readLines(filepath)
#' unlink(filepath)
#'
new_emitter <- function(..., targets = list(...)) {
  MultiEmitter$new(targets)
}
