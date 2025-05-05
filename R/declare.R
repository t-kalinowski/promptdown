#' Find the first tool() declaration in a function's top-level declare() calls
#' Extract a `tool()` declaration from a function
#'
#' Searches through a function's top-level `declare()` calls to find and return the first `tool()` declaration, if present.
#'
#' @param fn A function object to inspect.
#' @return The `tool(...)` call if found, otherwise `NULL`.
#' @keywords internal
#' @noRd
#' @examples
#' get_current_weather <- function(location = "") {
#'   declare(tool(
#'     "Get the current weather",
#'     location = type_string(
#'       "If missing, defaults to the current user location",
#'       required = TRUE
#'     )
#'   ))
#'   "sunny"
#' }
#' find_tool_declaration(get_current_weather)
find_tool_declaration <- function(fn) {
  if (!is.function(fn)) return()
  body <- fn_body(fn)
  exprs <- if (is_call(body, "{")) {
    call_args(body)
  } else {
    stop("body must start with {")
  }

  # Iterate over each top-level expression
  for (expr in exprs) {
    if (is_call(expr, "declare")) {
      for (declare_arg in call_args(expr)) {
        if (is_call(declare_arg, "tool")) {
          return(declare_arg)
        }
      }
    }
  }

  # No tool() declaration found
  NULL
}

# rstudioapi::readRStudioPreference("knit_working_dir", "project")
