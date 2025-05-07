#' Run a chat prompt sheet
#'
#' This calls `read_chat_sheet()`, uses the generated `ellmer::Chat` object to
#' submit a request to the LLM provider, and streams the response back into
#' originating document.
#'
#' @param path Path to the `chat.Rmd` file. If `NULL`, uses the current active
#'   document in RStudio.
#' @param stream Whether to stream the response as it's generated (`TRUE`) or
#'   return the full response at once (`FALSE`).
#'
#' @return Invisibly returns the updated `ellmer::Chat` object.
#'
#' @export
run_chat_sheet <- function(path = NULL, stream = TRUE) {
  if (is.null(path)) {
    rstudioapi::documentSave()
    path <- rstudioapi::getSourceEditorContext()$path
  }

  if (!endsWith(path, "chat.Rmd")) stop("Filepath must end in 'chat.Rmd'")

  turns_sheet_path <- new_turns_sheet_path()
  .[chat, next_turn_contents] <- read_chat_sheet(
    path,
    output = turns_sheet_path
  )

  emitter <- new_emitter(turns_sheet_path, path)
  emit <- emitter$emit
  on.exit(emitter$close())

  tools <- chat$get_tools()
  tools <- lapply(tools, trace_tool, emit)
  chat$set_tools(tools)

  if (!length(next_turn_contents)) {
    # No new user turn, we're overwriting the last LLM response

    # delete last LLM response. This modifies files!
    delete_last_llm_turn(turns_sheet_path)
    delete_last_llm_turn(path)

    turns <- chat$get_turns(include_system_prompt = TRUE)

    # drop last LLM turn
    turns <- drop_last(turns)

    # split off the last user turn
    next_turn_contents <- last(turns)@contents
    turns <- drop_last(turns)

    chat$set_turns(turns)
  }

  emit("\n\n# LLM\n\n")

  if (stream) {
    response <- do.call(chat$stream, next_turn_contents)
    coro::loop(for (output in response) emit(output))
  } else {
    response <- do.call(chat$chat, next_turn_contents)
    emit(response)
  }

  emit("\n\n# User\n\n")
  invisible(chat)
}


delete_last_llm_turn <- function(path) {
  lines <- readLines(path) |> stri_trim_right()

  # peel off empty trailing turns
  lines <- stri_flatten(lines, "\n") |> stri_trim_right()
  while (stri_detect_regex(lines, "\\s*\n# (LLM|User)\\s*$"))
    lines <- stri_replace_last_regex(lines, "\\s*\n# (LLM|User)\\s*$", "")
  lines <- stri_split_lines(lines)[[1L]]

  turn_start <- which(
    lines %in% c("# User", "System", "# LLM", "# Assistant")
  )

  # peel off last User Turn
  while (lines[last(turn_start)] == "# User")
    length(turn_start) <- length(turn_start) - 1L

  # collapse consecutive LLM Turns
  repeat {
    second_to_last_turn_start <- turn_start[length(turn_start) - 1]
    if (lines[second_to_last_turn_start] %in% c("# LLM", "# Assistant"))
      length(turn_start) <- length(turn_start) - 1L else break
  }

  lines <- lines[seq_len(last(turn_start) - 1L)]

  if (is_rstudio() || is_positron()) {
    context <- rstudioapi::getSourceEditorContext()
    if (normalizePath(path) == normalizePath(context$path)) {
      rstudioapi::setDocumentContents(stri_flatten(lines, "\n"))
      rstudioapi::documentSave()
      return()
    }
  }

  writeLines(lines, path)
}


addin_run_chat_sheet <- function() {
  for (cl in lapply(sys.calls(), `[[`, 1L)) {
    if (identical(cl, quote(promptdown::run_chat_sheet))) {
      later::later(
        function()
          rstudioapi::sendToConsole(
            "promptdown::run_chat_sheet()",
            focus = FALSE
          )
      )
      tools::pskill(Sys.getpid(), tools::SIGINT)
      return()
    }
  }

  rstudioapi::sendToConsole(
    "promptdown::run_chat_sheet()",
    focus = FALSE
  )
}
