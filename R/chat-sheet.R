#' Parse a prompt sheet and returns a dataframe with columns
#' - role: one of 'system' 'user' or assistant'
#' - turn: integer, the turn number in the sequence. System prompt if present is always 0.
#'   user turns are always odd, llm turns are even.
#' - text:
parse_chat_sheet <- function(file, text = readLines(file)) {
  text <- text |> stri_split_lines() |> unlist() |> stri_trim_right()

  # drop yaml frontmatter
  if (text[1] == "---") {
    text <- text[-seq_len(which(text == "---")[2])]
  }

  # segment
  df <- text |> markdown_frame("h1")

  # drop leading whitspace
  if (is.na(df$h1[1]) && stri_trim_both(df$text[1]) == "") df <- df[-1, ]

  # If we don't have a Turn start marker, assume document starts with User turn.
  if (is.na(df$h1[1])) df$h1[1] <- "# User"

  df <- df |>
    mutate(
      role = if_else(h1 %in% c("# System", "# User", "# LLM"), h1, NA) |>
        stri_replace_first_fixed("# ", "") |>
        stri_trans_tolower() |>
        replace_val("llm", "assistant") |>
        vctrs::vec_fill_missing(),
      turn = vctrs::vec_identify_runs(role),
      turn = if_else(role == "system", 0L, turn)
    ) |>
    group_by(role, turn) |>
    summarise(
      text = {
        # if there are other h1 headings (from the user),
        # then merge them back into the text
        text[-1] <- stri_c(h1[-1], text[-1])
        text |> stri_flatten() |> stri_trim_both()
      },
      .groups = "drop"
    ) |>
    arrange(turn)

  if (df$role[1] == "system" && df$text[1] == "") {
    # drop empty "system" prompt
    df <- df[-1, ]
    df$turn <- seq_along(df$turn)
  } else {
    # else system is always turn 0
    df$turn <- seq_along0(df$turn)
  }

  df
}

parse_tool_result <- function(x, request) {
  matches <- stri_match_first_regex(
    x,
    "^```([^\n]*)\n(.*)\n```$",
    dot_all = TRUE
  )
  type <- matches[, 2]
  value <- matches[, 3]

  if (type == "error") {
    return(ContentToolResult(error = value))
  }
  if (type == "json") {
    class(value) <- "json"
  } else if (type == "yaml") {
    value <- yaml::yaml.load(value)
  }

  ContentToolResult(value = value, request = request)
}

text = r"(
Describe this plot

``` r
plot(1:10)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)
![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-2.png)



# LLM

)"

sheet_into_turns <- function(x) {
  turns <- purrr::pmap(x, function(role, text, ...) {
    text <- markdown_segment(text, "pre")
    text <- unlist(imap(text, function(segment, name) {
      if (name == "pre") return(segment)
      img_link_locations <- stringi::stri_locate_all_regex(
        segment,
        "!\\[(?<alt>[^\\]]*)\\]\\((?<path>[^\\)]*)\\)",
        capture_groups = TRUE
      )[[1]]

      if (all(is.na(img_link_locations))) return(segment)
      cg <- attr(img_link_locations, "capture_groups")
      paths <- stri_sub(segment, cg$path)
      img_contents <- lapply(paths, ellmer::content_image_file)
      # alts <- stri_sub(segment, cg$alt)
      # stri_sub(segment, img_link_locations) <- sprintf(
      #   '(image with alt text: "%s")',
      #   alts
      # )
      c(segment, img_contents)
    }))
    # browser()

    # first convert images into ellmer Content blocks

    i <- 0L
    contents <- list()
    while (i < length(text)) {
      block <- text[[i <- i + 1L]]
      if (inherits(block, "ellmer::Content")) {
        append1(contents) <- block
        next
      }
      stopifnot(is.character(block))
      if (names(text)[i] == "pre") {
        if (stri_startswith_fixed(block, "``` tool")) {
          tool_request <- parse_tool_chunk(block)
          append1(contents) <- tool_request

          while (stri_trim_both(block <- text[i <- i + 1L]) == "") NULL
          tool_result <- parse_tool_result(block, tool_request)
          append1(contents) <- tool_result
          next
        }

        if (stri_startswith_fixed(block, "``` ellmer::Content")) {
          block <- stri_match_first_regex(
            block,
            "^```([^\n]*)\n(.*)```$",
            dot_all = TRUE
          )

          # .x <- stri_split_fixed(.x, "\n")[[1L]]
          constructor <- eval(str2lang(block[, 2]))
          args <- yaml::yaml.load(block[, 3])
          append1(contents) <- do.call(constructor, args)
          next
        }
      }

      append1(contents) <- ContentText(block)
    }
    # browser()
    ellmer::Turn(role, contents = contents)
  })

  # now make an effort to move ContentToolResult out of the llm turn and into a new user turn
  turns <- turns |>
    lapply(function(turn) {
      if (turn@role == "assistant") {
        is_res <- map_lgl(turn@contents, inherits, "ellmer::ContentToolResult")
        if (any(is_res)) {
          user_turn <- Turn("user", turn@contents[is_res])
          turn@contents <- turn@contents[!is_res]
          return(list(turn, user_turn))
        }
      }
      list(turn)
    }) |>
    unlist(recursive = FALSE)
  turns
}

new_chat_obj <- function(provider, ...) {
  switch(
    provider,
    ollama = ellmer::chat_ollama(...),
    openai = ellmer::chat_openai(...),
    stop("unsupported provider")
  )
}

map_lgl <- function(.x, .f, ...) vapply(X = .x, FUN = .f, FUN.VALUE = TRUE, ...)


do_chat <- function(provider, turns, tools, stream) {
}


submit_turns <- function(
  chat,
  turns,
  ...,
  stream = FALSE,
  provider = "ollama"
) {
  chat$set_turns(drop_last(turns))

  do_it <- if (stream) chat$stream else chat$chat
  do.call(do_it, last(turns)@contents)
}


Emitter <- R6::R6Class(
  "Emitter",
  public = list(
    target = NULL,

    connection = NULL,
    opened_connection = FALSE,
    rstudio_document_id = NULL,

    initialize = function(target = NULL) {
      self$target <- target

      if (is.null(target) || identical(target, "")) {
        self$connection <- stdout()
      } else if (is.character(target)) {
        id <- rstudioapi::documentId(allowConsole = FALSE)
        message("Using id: ", id)
        path <- normalizePath(rstudioapi::documentPath(id), mustWork = FALSE)

        if (normalizePath(target, mustWork = FALSE) == path) {
          self$rstudio_document_id <- id
        } else {
          self$connection <- file(target, "a")
          self$opened_connection <- TRUE
        }
      } else if (inherits(target, "connection")) {
        self$connection <- target
      } else if (is.function(target)) {
        # custom handler function - will be called directly
      } else {
        stop("target must be a file path, connection, or function")
      }
    },

    emit = function(msg) {
      if (!is.null(self$connection)) {
        cat(msg, file = self$connection, sep = "", append = TRUE)
      } else if (!is.null(self$rstudio_document_id)) {
        rstudioapi::insertText(
          location = Inf,
          msg,
          id = self$rstudio_document_id
        )
      } else if (is.function(self$target)) {
        browser()
        self$target(msg)
      }
    },

    close = function() {
      if (
        self$opened_connection &&
          inherits(self$connection, "connection") &&
          isOpen(self$connection)
      ) {
        close(self$connection)
        self$connection <- NULL
      }
      if (!is.null(self$rstudio_document_id)) {
        rstudioapi::documentSave(id = self$rstudio_document_id)
      }
    }
  ),

  private = list(
    finalize = function() {
      self$close()
    }
  )
)

MultiEmitter <- R6::R6Class(
  "MultiEmitter",
  public = list(
    emitters = NULL,

    initialize = function(xs) {
      # browser()
      self$emitters <- lapply(xs, Emitter$new)
    },

    emit = function(...) {
      msg <- stri_collapse(...)
      for (emitter in self$emitters) {
        # browser()
        emitter$emit(msg)
      }
    },

    close = function() {
      for (emitter in self$emitters) {
        emitter$close()
      }
    }
  )
)

new_emitter <- function(...) {
  MultiEmitter$new(c(...))
}


# S3 method for generic close()
#
# registerS3method(
#   "close",
#   "refinery::emitter",
#   function(x, ...) x$close()
# )

# stri_flatten()

# registerS3method("close", "refinery::emitter", function(x, ...) {
#   x$close()
# })

trace_tool <- function(tool, emit) {
  stopifnot(is.function(emit))
  og_fun <- tool@fun

  tool@fun <- function(...) {
    display_call <- stri_flatten_lines(
      deparse(as.call(list(as.symbol(tool@name), ...))),
      end_with_newline = TRUE
    )
    emit("```r\n", display_call, "```\n")
    withCallingHandlers(
      {
        result <- og_fun(...)

        display_result <- capture.output(print(result)) |>
          stri_flatten_lines(end_with_newline = TRUE)

        emit("```\n", display_result, "```\n\n")
        return(result)
      },
      error = function(e) {
        display_error <- capture.output(print(e)) |>
          stri_flatten_lines(end_with_newline = TRUE)

        emit("```error\n", display_error, "```\n\n")
      }
    )
    return(result)
  }
  tool
}

find_tools <- function(env) {
  calls <- drop_nulls(lapply(names(env), function(name) {
    obj <- get(name, env)
    if (!is.function(obj)) return()

    tool_cl <- find_tool_declaration(obj)
    if (is.null(tool_cl)) return()

    tool_cl[[".fun"]] <- obj
    tool_cl[[".name"]] <- name
    tool_cl
  }))

  withr::with_package("ellmer", {
    lapply(calls, eval, env)
  })
}


render_chat_sheet <- function(...) {
}

# run
# render
# submit
# post
# get
#' @export
run_chat_sheet <- function(
  path,
  update = TRUE,
  stream = update,
  log = TRUE,
  encoding = "UTF-8"
) {
  envir = new.env(parent = parent.frame())
  # envir <- new.env(parent = mask)
  # envir$knit_print <- knitr::knit_print
  # registerS3method(
  #   "knit_print",
  #   "knit_print.ellmer::Content",
  #   function(x, options, ...) {
  #     browser()
  #   },
  #   envir = envir
  # )
  # envir$`knit_print.ellmer::Content` <-
  # fm <- rmarkdown::yaml_front_matter(path)
  # envir$chat <- chat <- new_chat_obj(fm$provider, model = fm$model)
  # .GlobalEnv$chat <- chat

  fs::dir_create("_chats")
  md_path <- timestamp_filename(prefix = "_chats/", ext = ".md")
  # knitr::render_markdown()
  # TODO: install hooks to capture and encode figures
  tools <- content <- list()
  # envir$`knit_print.ellmer::ToolDef` <-
  # registerS3method(
  #   "knit_print",
  #   "ellmer::ToolDef",
  #   function(x, ...) {
  #     append1(tools) <<- x
  #     # NextMethod()
  #     ""
  #   },
  #   envir = asNamespace("knitr")
  # )
  # registerS3method(
  #   "knit_print",
  #   "ellmer::Content",
  #   function(x, ...) {
  #     append1(content) <<- x
  #     ellmer::contents_markdown(x) |> knitr::asis_output()
  #   },
  #   envir = asNamespace("knitr")
  # )

  knitr::render_markdown()
  knitr::opts_chunk$set(render = function(x, ...) {
    if (inherits(x, "ellmer::Content")) {
      if (inherits(x, "ellmer::ContentText")) {
        return(knitr::asis_output(x@text))
      }

      yaml <- yaml::as.yaml(S7::props(x))
      x <- stri_join("``` ", class(x)[1], "\n", yaml, "```")
      return(knitr::asis_output(x))
    }
    knitr::knit_print(x, ...)
  })
  # browser()
  # debug(knitr::knit_hooks$get("output"))
  # debug(knitr::knit_hooks$get("source"))
  # debug(knitr::knit_hooks$get("chunk"))

  knitr::knit(path, output = md_path, envir = envir)
  chat <- find_chat(envir)
  # readLines(md_path) |> writeLines()

  # browser()

  emitter <- new_emitter(c(if (update) path, md_path, stdout()))
  emit <- emitter$emit
  on.exit(emitter$close())

  tools <- find_tools(envir)
  tools <- lapply(tools, trace_tool, emitter$emit)
  tools |> lapply(chat$register_tool)

  turns <- md_path |> parse_chat_sheet() |> sheet_into_turns()

  emit("\n\n# LLM\n\n")

  response <- submit_turns(chat, turns, stream = stream, echo = "none")
  # browser()
  if (stream) coro::loop(for (output in response) emit(output)) else
    emit(response)

  emit("\n\n# User\n\n")
  invisible(chat)
}

format_content <- function(x) {
}

# coro::collect()
#
# new_turns <- chat$get_turns(include_system_prompt = TRUE)[-seq_along(turns)]
#
# new_turns <- new_turns |>
#   lapply(\(x) format_for_sheet(x@contents)) |>
#   unlist() |>
#   stri_flatten("\n\n")
#
# new_turns <- stri_c("\n\n# LLM\n\n", new_turns)
#
# new_turns |> cat(file = md_path, append = TRUE)
#
# if (update) {
#   id <- rstudioapi::documentOpen(
#     path,
#     line = .Machine$integer.max,
#     col = .Machine$integer.max,
#     moveCursor = TRUE
#   )
#   new_turns <- stri_c(new_turns, "\n\n# User\n\n")
#   rstudioapi::insertText(location = Inf, new_turns, id = id)
# }

update_chat_sheet <- function(path, new) {
  id <- rstudioapi::documentOpen(
    path,
    line = .Machine$integer.max,
    col = .Machine$integer.max,
    moveCursor = TRUE
  )

  if (stream) {
    coro::loop(
      for (text in resp) {
        rstudioapi::insertText(location = Inf, text, id = id)
      }
    )
  } else {
    rstudioapi::insertText(location = Inf, resp, id = id)
  }
}

if (FALSE) {
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


if (FALSE) {
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

  b <- stri_locate_all_regex(
    code,
    "\n```.*\n```\n",
    dot_all = TRUE,
    multi_line = TRUE
  )

  stri_sub(code, b[[1]])

  stri_split()

  rstudioapi::insertText(location = Inf, "foo", id = "9A0C2F1E")
}


write_job_script <- function(path) {
  id <- rstudioapi::documentOpen(path, moveCursor = FALSE)
  script_path <- tempfile(fileext = ".R")
  writeLines(
    glue(
      .open = "<<",
      .close = ">>",
      r"-----(
  library(stringi)
  library(dplyr)
  x <- ragnar:::parse_chat_sheet(r"-(<<path>>)-")
  stream <- ragnar:::get_completion(x, model = "gemma3:1b", stream = TRUE)
  coro::loop(for(text in stream) {
    rstudioapi::insertText(location = Inf, text, id = "<<id>>")
  })
  )-----"
    ),
    script_path
  )
  script_path
}

run_completion_job <- function(path) {
  script <- write_job_script(path)
  rstudioapi::jobRunScript(script, "completion", workingDir = getwd())
}


stream_into_document <- function(path, stream) {
  id <- rstudioapi::documentOpen(
    path,
    .Machine$integer.max,
    .Machine$integer.max,
    moveCursor = FALSE
  )
  stopifnot(is.function(stream)) # coro generator

  coro::loop(
    for (resp in stream) {
      # print(resp)
      rstudioapi::insertText(location = Inf, resp, id = id)
    }
  )

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
  # chat <- chat_ollama(model = "llama3.2", echo = "all")
  chat$register_tool(
    tool_def = tool(
      function() {
        cat("i was called")
        as.character(Sys.time())
      },
      "Get the current time",
      .name = "get_current_time"
    )
  )
  chat$get_turns()
  chat$chat("what time is it?", echo = FALSE)
  chat$get_turns()
}

#
# Emitter <- R6::R6Class(
#   "Emitter",
#   public = list(
#     initialize = function(...) {
#       self$file_args <- list(...)
#       self$conn <- do.call(file, file_args)
#     },
#     emit = function(...) {
#       writeLines(c(...), self$conn)
#     },
#     close = list()
#   )
# )
#
# Emitter <- function(...) {
#   conn <- file(...)
#   function(...) {
#     writeLines()
#   }
# }

#     writeLines(c("```r", deparse(sys.call()), "```", "```"), conn)
#     capture.output()
#     withCallingHandlers({
#       result <- og_fun(...)
#
#     }, error = function(e) {
#
#       result
#
#     }
#
#     writeLines(c("```", ))
#   }

# new_emitter <-
function(...) {
  conn <- ""
  env <- environment()
  emitters <- lapply(c(...), function(x) {
    if (is.character(x)) {
      # If the document is active in Rstudio, then use the rstudioapi
      # to stream it in
      id <- rstudioapi::documentId(allowConsole = FALSE)
      path <- normalizePath(rstudioapi::documentPath(id), mustWork = FALSE)
      if (normalizePath(x, mustWork = FALSE) == path) {
        emit <- function(...) {
          rstudioapi::insertText(
            location = Inf,
            stri_flatten(c(...), ""),
            id = id
          )
        }
        return(emit)
      }
      rm(id, path)

      # otherwise, use the standard file api
      conn <- file(x, "a")
      reg.finalizer(
        env,
        function(env) {
          cat("closing")
          print(conn)
          if (isOpen(conn)) close(conn)
        },
        TRUE
      )
    } else if (inherits(x, "connection")) {
      conn <- x
      if (!isOpen(conn, "write")) {
        open(conn, "a")
        reg.finalizer(
          env,
          function(env) {
            cat("closing")
            print(conn)
            if (isOpen(conn)) close(conn)
          },
          TRUE
        )
      }
    } else if (is.function(x)) {
      return(function(...) x(c(character(), ...)))
    } else {
      stop("x must be a filepath string or connection")
    }

    function(...) {
      cat(c(...), sep = "", file = conn, append = TRUE)
    }
  })

  connections <- drop_nulls(lapply(emitters, function(emitter) {
    get0("conn", environment(emitter), inherits = FALSE)
  }))

  # if (length(emitters) == 0) {
  #   emit <- function(...) NULL
  # } else if (length(emitters) == 1) {
  #   emit <- emitters[[1]]
  # } else {
  emit <- function(...) {
    x <- c(...)
    tryCatch(
      {
        for (emit in emitters) {
          emit(x)
        }
      },
      error = function(e) browser()
    )
    # }
  }
  class(emit) <- "refinery::emitter"
  emit
}

# registerS3method("close", "refinery::emitter", function(x) {
#   # browser()
#   for (conn in get("connections", environment(x))) if (isOpen(conn)) close(conn)
# })

`append1<-` <- function(x, value) {
  stopifnot(is.list(x) || identical(mode(x), mode(value)))
  x[[length(x) + 1L]] <- value
  x
}
# library(R6)
# library(R6)

text <- "
Some intro text.

``` ellmer::ContentPDF
type: application/pdf
data: ...
```

Some conclusion text.
"

if (FALSE) {
  doc <- commonmark::markdown_html(text, sourcepos = TRUE) |> xml2::read_html()
}

# library(vctrs)
# split_markdown_content_chunks(text)
