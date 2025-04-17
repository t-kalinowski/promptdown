
parse_chat_sheet <- function(file, text = readLines(file)) {
  text <-  text |> stri_split_lines() |> unlist() |> stri_trim_right()

  # drop yaml frontmatter
  if (text[1] == "---") {
    text <- text[-seq_len(which(text == "---")[2])]
  }

  # segment
  df <- text |> markdown_frame("h1")

  # drop leading whitspace
  if (is.na(df$h1[1]) && stri_trim_both(df$text[1]) == "")
    df <- df[-1, ]

  # If we don't have a Turn start marker, assume document starts with User turn.
  if (is.na(df$h1[1]))
    df$h1[1] <- "# User"

  df <- df |>
    mutate(
      role = if_else(h1 %in% c("# System", "# User", "# LLM"), h1, NA),
      role = role |>
        stri_replace_first_fixed("# ", "") |>
        stri_trans_tolower() |>
        replace_val("llm", "assistant") |>
        vctrs::vec_fill_missing(),
      turn = vctrs::vec_identify_runs(role),
      turn = if_else(role == "system", 0L, turn)
    ) |>
    group_by(role, turn) |>
    summarise(text = {
      text[-1] <- stri_c(h1[-1], text[-1])
      text |> stri_flatten() |> stri_trim_both()
    }, .groups = "drop") |>
    arrange(turn)


  if(df$role[1] == "system" && df$text[1] == "") {
    df <- df[-1,]
    df$turn <- seq_along(df$turn)
  } else {
    df$turn <- seq_along0(df$turn)
  }

  df
}

sheet_into_turns <- function(x) {
  turns <- purrr::pmap(x, function(role, text, ...) {
    ellmer::Turn(role, contents = list(
      ellmer::ContentText(text)
    ))
  })
  turns
}

get_chat_obj <- function(provider, ..., echo = "all") {
  switch(
    provider,
    ollama = ellmer::chat_ollama(turns = NULL, ...),
    openai = ellmer::chat_openai(turns = NULL, ...),
    stop("unsupported provider")
  )

}


submit_turns <- function(chat, turns, ..., stream = FALSE, provider = "ollama") {
  chat$set_turns(drop_last(turns))

  if (stream) {
    chat$stream(!!!last(turns)@contents)
  } else {
    chat$chat(!!!last(turns)@contents, ...)
  }

}



format_for_sheet <- S7::new_generic("format_for_sheet", "x")

S7::method(format_for_sheet, S7::class_list) <- function(x) {
  purrr::map_chr(x, format_for_sheet)
}

S7::method(format_for_sheet, ellmer::ContentToolResult) <- function(x) {
  if (is.null(x@error)) {
    stri_flatten(c(
      "```tool-call-result",
      stri_c("#| call-id: ", x@id),
      x@value,
      "```"
    ), "\n")
  } else {
    stri_flatten(c(
      "```tool-call-error",
      stri_c("#| call-id: ", x@id),
      x@error,
      "```"
    ), "\n"
    )
  }
}

S7::method(format_for_sheet, ellmer::ContentText) <- function(x) {
  x@text
}

S7::method(format_for_sheet, ellmer::ContentToolRequest) <- function(x) {
  cl <-
    rlang::call2(x@name, !!!x@arguments) |>
    rlang::expr_deparse(width = 30) |>
    stri_flatten("\n")
  stri_flatten_lines(
    "```tool-call",
    stri_c("#| call-id: ", x@id),
    cl,
    "```"
  )
}

#' @export
run_chat_sheet <- function(path, update = TRUE) {

  fm <- rmarkdown::yaml_front_matter(path)
  env <- new.env(parent = parent.frame())
  env$chat <- chat <- get_chat_obj(fm$provider, model = fm$model)

  fs::dir_create("_chats")
  md_path <- timestamp_filename(prefix = "_chats/", ext = ".md")
  knitr::knit(path, output = md_path, envir = env)

  turns <- md_path |> parse_chat_sheet() |> sheet_into_turns()
  # browser()
  resp <- submit_turns(chat, turns) #, echo = "all")
  # resp

  new_turns <- chat$get_turns(include_system_prompt = TRUE)[-seq_along(turns)]

  new_turns <- new_turns |>
    lapply(\(x) format_for_sheet(x@contents)) |>
    unlist() |>
    stri_flatten("\n\n")

  new_turns <- stri_c("\n\n# LLM\n\n", new_turns)

  new_turns |> cat(file = md_path, append = TRUE)

  if (update) {
    id <- rstudioapi::documentOpen(
      path,
      line = .Machine$integer.max,
      col = .Machine$integer.max,
      moveCursor = TRUE
    )
    new_turns <- stri_c(new_turns, "\n\n# User\n\n")
    rstudioapi::insertText(location = Inf, new_turns, id = id)
  }

  invisible(md_path)
}



update_chat_sheet <- function(path, new) {

  id <- rstudioapi::documentOpen(
    path,
    line = .Machine$integer.max,
    col = .Machine$integer.max,
    moveCursor = TRUE
  )

  if(stream) {
    coro::loop(for(text in resp) {
      rstudioapi::insertText(location = Inf, text, id = id)
    })
  } else {
    rstudioapi::insertText(location = Inf, resp, id = id)
  }
}

if(FALSE) {
  run_chat_sheet("test-chat.chat.md")
}


append_completion <- function(file) {
  # print(file)
  # print(code)

  stri_locate_all_boundaries("\n```.*\n```\n")

  # #!source ragnar:::chat_sheet(.file, .code)

  # ---
  # knit: ({source(here::here("tools/knit.R")); knit_chat})
  # ---
}


if(FALSE) {

  library(stringi)
  library(dplyr)
  path <- "test-chat.chat.md"
  x <- parse_chat_sheet("test-chat.chat.md")
  completion <- get_completion(x, model = "gemma3:1b", stream = TRUE)

  stream_into_document(file, completion)

  cat("\n\n# LLM\n\n", completion, "\n", sep = "", file = file, append = TRUE)

  rstudioapi::getActiveDocumentContext()

  chat$last_turn()
  chat$chat()


  code <- readLines("test-chat-sheet.Rmd") |> stri_flatten("\n")

  b <- stri_locate_all_regex(code, "\n```.*\n```\n", dot_all = TRUE, multi_line = TRUE)

  stri_sub(code, b[[1]])

  stri_split()

  rstudioapi::insertText(location = Inf, "foo", id = "9A0C2F1E")

}


write_job_script <- function(path) {
  id <- rstudioapi::documentOpen(path, moveCursor = FALSE)
  script_path <- tempfile(fileext = ".R")
  writeLines(glue(.open = "<<", .close = ">>", r"-----(
  library(stringi)
  library(dplyr)
  x <- ragnar:::parse_chat_sheet(r"-(<<path>>)-")
  stream <- ragnar:::get_completion(x, model = "gemma3:1b", stream = TRUE)
  coro::loop(for(text in stream) {
    rstudioapi::insertText(location = Inf, text, id = "<<id>>")
  })
  )-----"), script_path)
  script_path
}

run_completion_job <- function(path) {
  script <- write_job_script(path)
  rstudioapi::jobRunScript(script, "completion", workingDir = getwd())
}


stream_into_document <- function(path, stream) {
  id <- rstudioapi::documentOpen(path, .Machine$integer.max, .Machine$integer.max,
                                 moveCursor = FALSE)
  stopifnot(is.function(stream)) # coro generator

  coro::loop(for(resp in stream) {
    # print(resp)
    rstudioapi::insertText(location = Inf, resp, id = id)
  })

  # later::later(function() {
  #   text <- stream()
  #   if (is.null(text))
  #     return()
  #   rstudioapi::insertText(location = Inf, text, id = id)
  #   later::later(sys.function(), delay = 1)
  # }, delay = 1)


}





#
# chat_sheet <- function(file, code) {
#   print(file)
#   print(code)
#
#   stri_locate_all_boundaries("\n```.*\n```\n")
#
#   # #!source ragnar:::chat_sheet(.file, .code)
#
#   # ---
#   # knit: ({source(here::here("tools/knit.R")); knit_chat})
#   # ---
# }


if (FALSE) {
  httr2::local_verbosity(2)
  library(ellmer)
  # chat <- chat_openai()
  chat <- chat_ollama(model =  "llama3.2", echo = "all")
  chat$register_tool(tool_def = tool(
    function() {
      cat("i was called")
      as.character(Sys.time())
    }, "Get the current time", .name = "get_current_time"
  ))
  chat$get_turns()
  chat$chat("what time is it?", echo = FALSE)
  chat$get_turns()

}
