#' Write Chat as HTML
#'
#' Converts an ellmer::Chat object to a self-contained HTML file that can be viewed in a browser.
#'
#' @param chat An ellmer::Chat object containing the conversation
#' @param output_path Path where to save the HTML file. If NULL, returns the HTML as a string
#' @param theme Visual theme to use. Options are "light" or "dark" (ignored in minimal mode)
#' @param embed_images Logical. If TRUE, images will be embedded as base64 data URLs
#'
#' @return Invisibly, the HTML content as a string
#'
#' @export
write_chat_html <- function(chat, output_path = NULL, theme = "light", embed_images = TRUE) {
  # Try to get turns from the chat object
  turns <- tryCatch({
    # Most Chat objects have a get_turns method
    if (is.function(chat$get_turns)) {
      chat$get_turns(include_system_prompt = TRUE)
    } else if (is.list(chat)) {
      # Assume it's already a list of turns
      chat
    } else {
      stop("Cannot extract turns from chat object")
    }
  }, error = function(e) {
    stop("Could not get turns from the chat object. Make sure it has a 'get_turns' method or pass a list of turns directly.")
  })
  
  # Add debug info about the chat object
  message("Processing chat with ", length(turns), " turns")
  
  # Debug print first turn structure
  if (length(turns) > 0) {
    first_turn <- turns[[1]]
    message("First turn class: ", paste(class(first_turn), collapse = ", "))
    
    # Try to access contents
    contents <- tryCatch({
      if (is.list(first_turn$contents)) {
        first_turn$contents
      } else {
        # Try direct attribute access as fallback
        first_turn@contents
      }
    }, error = function(e) NULL)
    
    message("Contents count: ", length(contents))
    
    if (length(contents) > 0) {
      message("First content class: ", paste(class(contents[[1]]), collapse = ", "))
    }
  }
  
  # Convert turns directly to HTML
  html <- turns_to_html(turns, embed_images)
  
  # Write to file if path is provided
  if (!is.null(output_path)) {
    writeLines(html, output_path)
  }
  
  invisible(html)
}

#' Convert Chat Turns Directly to HTML
#' 
#' @param turns List of turn objects
#' @param embed_images Logical. If TRUE, images will be embedded as base64 data URLs
#' @return Complete HTML document as a string
turns_to_html <- function(turns, embed_images = TRUE) {
  # Generate a title based on timestamp
  title <- paste("Chat -", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
  # Initialize HTML content
  turn_html_parts <- c()
  
  # Process each turn
  for (turn in turns) {
    # Print debug info for this turn
    if (TRUE) {
      message("Turn class: ", paste(class(turn), collapse = ", "))
    }
    
    # Get role - directly check for common patterns
    role <- "unknown"
    
    # Try as a list/environment
    if (is.list(turn) || is.environment(turn)) {
      if (!is.null(turn$role)) {
        if (is.character(turn$role)) {
          role <- turn$role
        } else if (is.function(turn$role)) {
          role <- turn$role()
        }
      }
    }
    
    # If still unknown, try direct attribute access
    if (role == "unknown") {
      tryCatch({
        role_val <- eval(parse(text = "turn@role"))
        if (!is.null(role_val) && is.character(role_val)) {
          role <- role_val
        }
      }, error = function(e) {})
    }
    
    # If still unknown, try introspecting the object
    if (role == "unknown" && any(c("system", "user", "assistant") %in% names(turn))) {
      for (possible_role in c("system", "user", "assistant")) {
        if (!is.null(turn[[possible_role]]) && turn[[possible_role]]) {
          role <- possible_role
          break
        }
      }
    }
    
    # Simple role display
    role_display <- switch(role,
      "system" = "System",
      "user" = "User",
      "assistant" = "Assistant",
      tolower(role)
    )
    
    # Create turn container with alignment based on role
    # Right-align user messages
    align_style <- if (role == "user") ' style="text-align: right;"' else ''
    
    turn_html <- paste0(
      '<div data-role="', html_escape(role), '"', align_style, '>',
      '<h2>', role_display, '</h2>',
      '<div>'
    )
    
    # Get contents (from either turn$contents or turn@contents)
    contents <- list()
    
    # Check for S7 objects specially
    if (inherits(turn, c("S7_object", "ellmer::"))) {
      message("Processing S7 turn - direct access contents")
      
      # Try direct slot access for S7 objects
      tryCatch({
        # Use direct slot access without checking first
        contents_val <- turn@contents
        message("S7 contents access successful, length: ", length(contents_val))
        
        # If it's a list, use it directly
        if (is.list(contents_val)) {
          contents <- contents_val
          message("Contents is a list, length: ", length(contents))
        } else if (is.character(contents_val)) {
          # If it's a character string, wrap it
          contents <- list(list(text = contents_val))
          message("Contents is a character string, wrapped as text")
        } else {
          message("Contents is another type: ", class(contents_val))
        }
      }, error = function(e) {
        message("S7 contents access error: ", e$message)
      })
    } 
    # Try as a list/environment if not S7 or if S7 failed
    else if (is.list(turn) || is.environment(turn)) {
      if (!is.null(turn$contents)) {
        if (is.list(turn$contents)) {
          contents <- turn$contents
        } else if (is.character(turn$contents)) {
          # If it's a simple character string, wrap it
          contents <- list(list(text = turn$contents))
        }
      }
    }
    
    # If still empty, check for common content patterns
    if (length(contents) == 0) {
      # Look for content, text, or message fields
      for (field in c("content", "text", "message")) {
        if (!is.null(turn[[field]]) && !is.function(turn[[field]])) {
          contents <- list(list(text = as.character(turn[[field]])))
          break
        }
      }
    }
    
    # If we still have no contents, create a placeholder
    if (length(contents) == 0) {
      contents <- list(list(text = paste("Turn with role:", role_display, "- no content available")))
    }
    
    # Process each content in the turn
    for (content in contents) {
      turn_html <- paste0(turn_html, content_to_html(content, embed_images))
    }
    
    # Close turn container
    turn_html <- paste0(turn_html, '</div></div>')
    turn_html_parts <- c(turn_html_parts, turn_html)
  }
  
  # Combine all turns HTML
  content_html <- paste(turn_html_parts, collapse = "\n\n")
  
  # Create minimal HTML document without custom CSS
  html_document <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "  <meta charset=\"UTF-8\">\n",
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n",
    "  <title>", title, "</title>\n",
    "</head>\n",
    "<body>\n",
    "  <div>\n",
    "    <h1>", title, "</h1>\n",
    "    <div>\n",
    content_html,
    "\n    </div>\n",
    "  </div>\n",
    "</body>\n",
    "</html>"
  )
  
  return(html_document)
}

#' Convert a Single Content Object to HTML
#' 
#' @param content A content object or list
#' @param embed_images Whether to embed images as base64 data URLs
#' @return HTML string representation
content_to_html <- function(content, embed_images = TRUE) {
  # Handle text content
  if (is_text_content(content)) {
    text <- get_text_content(content)
    text_html <- commonmark::markdown_html(text)
    return(paste0('<div data-type="text">', text_html, '</div>'))
  }
  
  # Handle image content
  if (is_image_content(content)) {
    # Try to get image path or data
    path <- get_image_path(content)
    if (!is.null(path) && path != "") {
      # Handle file-based image
      img_src <- if (embed_images && file.exists(path)) {
        # Embed as base64
        mime_type <- get_mime_type(path)
        base64_data <- base64_encode_file(path)
        paste0("data:image/", mime_type, ";base64,", base64_data)
      } else {
        # Use path directly
        html_escape(path)
      }
      
      return(paste0(
        '<div data-type="image">',
        '<img alt="" src="', img_src, '">',
        '</div>'
      ))
    }
    
    # Check for direct base64 data
    data <- get_image_data(content)
    if (!is.null(data) && data != "") {
      img_src <- if (grepl("^data:image/", data)) {
        # Already a data URL
        data
      } else {
        # Raw base64 data
        paste0("data:image/png;base64,", data)
      }
      
      return(paste0(
        '<div data-type="image">',
        '<img alt="" src="', img_src, '">',
        '</div>'
      ))
    }
    
    # No valid image data found
    return('<div data-type="image">(image not available)</div>')
  }
  
  # Handle tool request/result content
  if (has_property(content, "name") && has_property(content, "arguments")) {
    # Tool request
    name <- get_property(content, "name", "unknown")
    args <- get_property(content, "arguments", list())
    id <- get_property(content, "id", "")
    
    # Format tool call
    call_text <- format_tool_call(name, args)
    
    return(paste0(
      '<div data-type="tool-request" data-tool-id="', html_escape(id), 
      '" data-tool-name="', html_escape(name), '">',
      '<pre><code>', html_escape(call_text), '</code></pre>',
      '</div>'
    ))
  }
  
  if (has_property(content, "value") || has_property(content, "error")) {
    # Tool result
    id <- get_property(content, "id", "")
    error <- get_property(content, "error", NULL)
    value <- get_property(content, "value", "")
    
    if (is.null(error)) {
      return(paste0(
        '<div data-type="tool-result" data-tool-id="', html_escape(id), '">',
        '<pre><code>', html_escape(value), '</code></pre>',
        '</div>'
      ))
    } else {
      return(paste0(
        '<div data-type="tool-error" data-tool-id="', html_escape(id), '">',
        '<pre><code>', html_escape(error), '</code></pre>',
        '</div>'
      ))
    }
  }
  
  # Generic content fallback - try to convert to YAML
  if (is.list(content)) {
    # Filter out functions and environments
    content_safe <- lapply(content, function(item) {
      if (is.function(item) || is.environment(item)) return(NULL)
      item
    })
    content_safe <- content_safe[!sapply(content_safe, is.null)]
    
    if (length(content_safe) > 0) {
      # Convert to YAML or plain text
      content_text <- tryCatch(
        yaml::as.yaml(content_safe),
        error = function(e) paste(capture.output(print(content_safe)), collapse = "\n")
      )
      
      return(paste0(
        '<div data-type="generic">',
        '<pre><code>', html_escape(content_text), '</code></pre>',
        '</div>'
      ))
    }
  }
  
  # If we got here, we couldn't determine the content type
  return(paste0(
    '<div data-type="unknown">',
    '<pre><code>', html_escape(paste("Unknown content type")), '</code></pre>',
    '</div>'
  ))
}

# Helper functions - with special S7 handling

is_text_content <- function(content) {
  # Direct check for S7 ContentText objects
  if (inherits(content, c("ellmer::ContentText", "ContentText", "S7_object"))) {
    message("Found S7 content text object")
    return(TRUE)
  }
  
  # Regular checks
  if (is.character(content)) return(TRUE)
  if (has_property(content, "text")) return(TRUE)
  FALSE
}

get_text_content <- function(content) {
  # Direct handling for S7 ContentText objects
  if (inherits(content, c("ellmer::ContentText", "ContentText", "S7_object"))) {
    message("Extracting text from S7 content text object")
    tryCatch({
      text_val <- content@text
      if (!is.null(text_val)) return(as.character(text_val))
    }, error = function(e) {
      message("Error accessing S7 content text: ", e$message)
    })
  }
  
  # Regular handling
  if (is.character(content)) return(content)
  if (has_property(content, "text")) return(as.character(get_property(content, "text")))
  
  # Don't try to convert non-character content directly
  # Instead return a placeholder
  "Content not available as text"
}

is_image_content <- function(content) {
  # Look for typical image properties
  for (prop in c("path", "uri", "url", "src", "source", "base64", "data", "image_data")) {
    if (has_property(content, prop)) return(TRUE)
  }
  FALSE
}

get_image_path <- function(content) {
  # Try various properties that might hold a path
  for (prop in c("path", "uri", "url", "src", "source")) {
    if (has_property(content, prop)) {
      val <- get_property(content, prop)
      if (!is.null(val) && val != "") return(as.character(val))
    }
  }
  NULL
}

get_image_data <- function(content) {
  # Try various properties that might hold image data
  for (prop in c("base64", "data", "image_data", "content")) {
    if (has_property(content, prop)) {
      val <- get_property(content, prop)
      if (!is.null(val) && val != "") return(as.character(val))
    }
  }
  NULL
}

format_tool_call <- function(name, args) {
  tryCatch({
    if (is.call(args)) {
      # If args is already a call, deparse it
      deparse(args, width.cutoff = 60)
    } else {
      # Try to build a call and deparse it
      rlang::call2(name, !!!args) |>
        rlang::expr_deparse(width = 60) |>
        paste(collapse = "\n")
    }
  }, error = function(e) {
    paste("Tool call:", name, "\nArguments:", 
          paste(capture.output(print(args)), collapse = "\n"))
  })
}

# Generic property access helper functions - simplest possible approach

has_property <- function(obj, prop) {
  # For S7 objects, don't try to use [[
  if (inherits(obj, c("S7_object", "ellmer::"))) {
    # Use direct attribute access safely
    tryCatch({
      val <- eval(parse(text = paste0("obj@", prop)))
      !is.null(val)
    }, error = function(e) FALSE)
  } else if (is.list(obj) || is.environment(obj)) {
    # For lists/environments, use direct access
    !is.null(obj[[prop]])
  } else {
    # For other objects, try to be lenient
    tryCatch({
      val <- obj[[prop]]
      !is.null(val)
    }, error = function(e) FALSE)
  }
}

get_property <- function(obj, prop, default = NULL) {
  # For S7 objects, don't try to use [[
  if (inherits(obj, c("S7_object", "ellmer::"))) {
    # Use direct attribute access safely
    tryCatch({
      val <- eval(parse(text = paste0("obj@", prop)))
      if (!is.null(val)) return(val)
      default
    }, error = function(e) default)
  } else if (is.list(obj) || is.environment(obj)) {
    # For lists/environments, use direct access
    if (!is.null(obj[[prop]])) return(obj[[prop]])
    default
  } else {
    # For other objects, try to be lenient
    tryCatch({
      val <- obj[[prop]]
      if (!is.null(val)) return(val)
      default
    }, error = function(e) default)
  }
}

#' Escape HTML special characters
#' 
#' @param text Text to escape
#' @return Escaped text safe for HTML
html_escape <- function(text) {
  if (is.null(text)) return("")
  text <- as.character(text)
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub('"', "&quot;", text)
  text <- gsub("'", "&#39;", text)
  return(text)
}

#' Base64 encode a file
#' 
#' @param path Path to the file to encode
#' @return Base64 encoded string
base64_encode_file <- function(path) {
  if (!file.exists(path)) {
    warning("File does not exist: ", path)
    return("")
  }
  
  # Use base64enc package for encoding
  contents <- readBin(path, "raw", file.info(path)$size)
  base64_string <- base64enc::base64encode(contents)
  return(base64_string)
}

#' Get MIME type from file extension
#' 
#' @param path File path
#' @return MIME type string
get_mime_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  
  # Map common extensions to MIME types
  mime_types <- list(
    png = "png",
    jpg = "jpeg",
    jpeg = "jpeg",
    gif = "gif",
    svg = "svg+xml",
    webp = "webp",
    bmp = "bmp"
  )
  
  # Return the MIME type or default to png
  return(mime_types[[ext]] %||% "png")
}