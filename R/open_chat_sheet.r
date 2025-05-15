#' Create a new Chat Sheet
#'
#' Open a new chat sheet file.
#'
#' @param path `character(1)` Destination path for the new R Markdown file. Use
#'   `NULL` (default) to let the function choose an available filename in the
#'   working directory.
#'
#' @return Invisibly returns the path to the file that was created.
#' @export
open_chat_sheet <- function(path = "chat.Rmd") {
  if (file.exists(path)) {
    if (is_rstudio() || is_positron()) {
      rstudioapi::navigateToFile(path)
    } else {
      file.edit(path)
    }
    return(invisible(path))
  }

  writeLines(
    r"---(
```{r, echo = FALSE}
knitr::opts_chunk$set(comment = '', echo = FALSE)
chat <- ellmer::chat_openai(model = "gpt-4.1-mini")
# chat$set_tools(btw::btw_tools(tools = "docs"))
```

# System

You are an expert R programmer and mentor.

# User

)---",
    path
  )

  if (is_rstudio() || is_positron()) {
    rstudioapi::navigateToFile(path, line = Inf, column = Inf)
  } else {
    file.edit(path)
  }

  invisible(path)
}


get_unique_chat_sheet_path <- function(path = NULL) {
  if (is.null(path)) {
    path <- "chat.Rmd"
    while (file.exists(path)) {
      i <- sub(".*-([0-9]+).Rmd$", "\\1", path)
      if (i == path) {
        # no number suffix
        path <- sub("\\.Rmd$", "-1.Rmd", path)
      } else {
        # increment number suffix
        i <- as.integer(i) + 1L
        path <- sub("-([0-9]+).Rmd$", paste0("-", i, ".Rmd"), path)
      }
    }
  }
  path
}
