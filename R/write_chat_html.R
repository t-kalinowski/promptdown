#' Write Chat as HTML
#'
#' Converts an ellmer::Chat object to a self-contained HTML file that can be viewed in a browser.
#' The HTML file contains all the styling and formatting needed to display the chat conversation,
#' similar to how it's displayed in the chat viewer app.
#'
#' @param chat An ellmer::Chat object containing the conversation
#' @param output_path Path where to save the HTML file. If NULL, returns the HTML as a string
#' @param theme Visual theme to use. Options are "light" or "dark"
#' @param embed_images Logical. If TRUE, images will be embedded as base64 data URLs
#'
#' @return Invisibly, the HTML content as a string
#'
#' @export
write_chat_html <- function(chat, output_path = NULL, theme = "light", embed_images = TRUE) {
  # Validate the chat object
  if (!(is.list(chat) || inherits(chat, c("ellmer::Chat", "Chat", "R6")))) {
    stop("'chat' must be an ellmer::Chat object or a list of turns")
  }
  
  # Special case for a Chat that we know about
  turns <- NULL
  
  # Handle known chat object types
  if (inherits(chat, "Chat") && inherits(chat, "R6")) {
    # We have an R6 Chat object - try to extract turns directly
    if (is.function(chat$get_turns)) {
      turns <- tryCatch(
        chat$get_turns(include_system_prompt = TRUE),
        error = function(e) NULL
      )
    }
  } 
  
  # If we don't have turns yet, try the general approach
  if (is.null(turns)) {
    turns <- if (is.list(chat) && !is.function(chat$get_turns)) {
      chat  # Assume it's already a list of turns
    } else {
      # Try to get turns from a Chat-like object with get_turns method
      tryCatch(
        chat$get_turns(include_system_prompt = TRUE),
        error = function(e) {
          stop("Could not get turns from the chat object. ",
              "Make sure it has a 'get_turns' method or pass a list of turns directly.", 
              call. = FALSE)
        }
      )
    }
  }
  
  # Add debug info about the chat object class
  message("Processing chat object of class: ", paste(class(chat), collapse = ", "))
  message("Number of turns: ", length(turns))
  
  # Convert turns directly to HTML
  html <- turns_to_html(turns, theme, embed_images)
  
  # Write to file if path is provided
  if (!is.null(output_path)) {
    writeLines(html, output_path)
  }
  
  invisible(html)
}

#' Convert Chat Turns Directly to HTML
#' 
#' @param turns List of ellmer::Turn objects
#' @param theme "light" or "dark"
#' @param embed_images Logical. If TRUE, images will be embedded as base64 data URLs
#' @return Complete HTML document as a string
turns_to_html <- function(turns, theme = "light", embed_images = TRUE) {
  # Get the appropriate CSS based on theme
  css <- if (theme == "dark") dark_theme_css else light_theme_css
  
  # Generate a title based on timestamp
  title <- paste("Chat -", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  
  # Initialize HTML content
  turn_html_parts <- c()
  
  # Process each turn
  for (turn in turns) {
    # Get turn role and create heading - using safer access method
    role <- tryCatch({
      # First try S7/S4 slot access - this is the most common for ellmer objects
      if (inherits(turn, c("S7_object", "S4"))) {
        turn@role
      } else if (is.function(turn$role)) {
        turn$role()
      } else if (is.character(turn$role)) {
        turn$role
      } else {
        "unknown"
      }
    }, error = function(e) {
      "unknown"
    })
    
    role_display <- switch(role,
      "system" = "System",
      "user" = "User",
      "assistant" = "Assistant",
      tolower(role)
    )
    
    heading_class <- switch(role,
      "system" = "system-heading",
      "user" = "user-heading",
      "assistant" = "llm-heading"
    )
    
    # Create turn container with data attributes
    turn_html <- paste0(
      '<div class="turn" data-role="', html_escape(role), '">',
      '<h1 class="', heading_class, '">', role_display, '</h1>',
      '<div class="turn-content">'
    )
    
    # Get contents - handle different object types
    contents <- tryCatch({
      # First try S7/S4 slot access - this is the most common for ellmer objects
      if (inherits(turn, c("S7_object", "S4"))) {
        turn@contents
      } else if (is.list(turn$contents)) {
        turn$contents
      } else {
        list()
      }
    }, error = function(e) {
      list()
    })
    
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
  
  # Create the complete HTML document
  html_document <- paste0(
    "<!DOCTYPE html>\n",
    "<html>\n",
    "<head>\n",
    "  <meta charset=\"UTF-8\">\n",
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n",
    "  <title>", title, "</title>\n",
    "  <style>\n", css, "\n  </style>\n",
    "</head>\n",
    "<body>\n",
    "  <div class=\"container\">\n",
    "    <h1 class=\"chat-title\">", title, "</h1>\n",
    "    <div class=\"chat-content\">\n",
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
#' @param content An ellmer Content object or simple list with content properties
#' @param embed_images Whether to embed images as base64 data URLs
#' @return HTML string representation
content_to_html <- function(content, embed_images = TRUE) {
  # Detect content type and properties directly
  if (is.list(content)) {
    # Handle simple list content
    
    # Check if it's text content
    if (!is.null(content$text)) {
      text <- content$text
      if (!is.character(text)) {
        text <- as.character(text)
      }
      text_html <- commonmark::markdown_html(text)
      return(paste0(
        '<div class="content-text" data-type="text">',
        text_html,
        '</div>'
      ))
    }
    
    # Check if it's image content by looking for various possible image properties
    # First check for path/URI
    image_path <- NULL
    for (prop_name in c("path", "uri", "url", "src", "source")) {
      if (!is.null(content[[prop_name]]) && content[[prop_name]] != "") {
        image_path <- as.character(content[[prop_name]])
        break
      }
    }
    
    if (!is.null(image_path)) {
      if (image_path != "") {
        # Generate image source - either embed as base64 or use file path
        img_src <- if (embed_images && file.exists(image_path)) {
          paste0("data:image/", get_mime_type(image_path), ";base64,", 
                 base64_encode_file(image_path))
        } else {
          html_escape(image_path)
        }
        
        # Return HTML image tag with proper styling
        return(paste0(
          '<div class="content-image" data-type="image">',
          '<img alt="" src="', img_src, '" style="max-width:100%;display:block;margin:10px 0;">',
          '</div>'
        ))
      } else {
        return('<div class="content-image" data-type="image">(image path is empty)</div>')
      }
    }
    
    # Check for direct base64/data content
    image_data <- NULL
    for (prop_name in c("base64", "data", "image_data", "content")) {
      if (!is.null(content[[prop_name]]) && content[[prop_name]] != "") {
        image_data <- as.character(content[[prop_name]])
        break
      }
    }
    
    if (!is.null(image_data)) {
      # If we have base64 data, use it directly
      img_src <- if (grepl("^data:image/", image_data)) {
        # Already a data URL
        image_data
      } else {
        # Assume it's raw base64 data
        paste0("data:image/png;base64,", image_data)
      }
      
      return(paste0(
        '<div class="content-image" data-type="image">',
        '<img alt="" src="', img_src, '" style="max-width:100%;display:block;margin:10px 0;">',
        '</div>'
      ))
    }
    
    # Check if it's clearly an image type by its class or type
    if (!is.null(content$type) && grepl("image", content$type, ignore.case = TRUE)) {
      # This is an image type but we couldn't find path or data
      return('<div class="content-image" data-type="image">(image data not found)</div>')
    }
    
    # Check if it's tool request content
    if (!is.null(content$name) && !is.null(content$arguments)) {
      name <- as.character(content$name)
      args <- content$arguments
      id <- if (!is.null(content$id)) as.character(content$id) else ""
      
      # Format tool call
      call_text <- tryCatch({
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
      
      return(paste0(
        '<div class="content-tool-request" data-type="tool-request" data-tool-id="', 
        html_escape(id), '" data-tool-name="', html_escape(name), '">',
        '<pre class="code-block"><code>', html_escape(call_text), '</code></pre>',
        '</div>'
      ))
    }
    
    # Check if it's tool result content
    if (!is.null(content$value) || !is.null(content$error)) {
      id <- if (!is.null(content$id)) as.character(content$id) else ""
      
      if (is.null(content$error)) {
        value <- if (is.character(content$value)) content$value else as.character(content$value)
        return(paste0(
          '<div class="content-tool-result" data-type="tool-result" data-tool-id="', 
          html_escape(id), '">',
          '<pre class="tool-result"><code>', html_escape(value), '</code></pre>',
          '</div>'
        ))
      } else {
        error <- if (is.character(content$error)) content$error else as.character(content$error)
        return(paste0(
          '<div class="content-tool-error" data-type="tool-error" data-tool-id="', 
          html_escape(id), '">',
          '<pre class="tool-error"><code>', html_escape(error), '</code></pre>',
          '</div>'
        ))
      }
    }
    
    # For other list-based content, create a generic representation
    # Filter out functions and convert to YAML
    content_safe <- lapply(content, function(item) {
      if (is.function(item) || is.environment(item)) return(NULL)
      item
    })
    content_safe <- content_safe[!sapply(content_safe, is.null)]
    
    if (length(content_safe) > 0) {
      content_yaml <- tryCatch(
        yaml::as.yaml(content_safe),
        error = function(e) paste(capture.output(print(content_safe)), collapse = "\n")
      )
      
      return(paste0(
        '<div class="content-other" data-type="generic">',
        '<pre class="code-block"><code>', html_escape(content_yaml), '</code></pre>',
        '</div>'
      ))
    } else {
      return('<div class="content-empty">(empty content)</div>')
    }
  } else if (is.character(content)) {
    # If content is just a character string, treat it as text
    text_html <- commonmark::markdown_html(content)
    return(paste0(
      '<div class="content-text" data-type="text">',
      text_html,
      '</div>'
    ))
  } else if (inherits(content, c("S7_object", "S4"))) {
    # Try to extract S7/S4 object content
    
    # Try to get content class
    content_class <- class(content)[1]
    content_type <- gsub("^.*::", "", content_class)
    
    # Check for specific known types
    if (grepl("ContentText", content_class)) {
      # Get text and convert to HTML
      text <- tryCatch(content@text, error = function(e) as.character(content))
      text_html <- commonmark::markdown_html(text)
      return(paste0(
        '<div class="content-text" data-type="text">',
        text_html,
        '</div>'
      ))
    } else if (grepl("ContentImage", content_class) || grepl("image", content_class, ignore.case = TRUE)) {
      # Get image path and create image tag - try all possible property names
      path <- tryCatch({
        # Try all possible property names for the path using slot access
        for (prop_name in c("path", "uri", "url", "src", "source")) {
          # Use a safer way to check for slots
          if (prop_name %in% methods::slotNames(content)) {
            path_value <- methods::slot(content, prop_name)
            if (!is.null(path_value) && path_value != "") {
              return(path_value)
            }
          }
        }
        NULL
      }, error = function(e) NULL)
      
      if (!is.null(path) && path != "") {
        # Generate image source
        img_src <- if (embed_images && file.exists(path)) {
          paste0("data:image/", get_mime_type(path), ";base64,", 
                 base64_encode_file(path))
        } else {
          html_escape(path)
        }
        
        return(paste0(
          '<div class="content-image" data-type="image">',
          '<img alt="" src="', img_src, '" style="max-width:100%;display:block;margin:10px 0;">',
          '</div>'
        ))
      } else {
        # Check if we already have a base64 data URI directly in the object
        img_data <- tryCatch({
          for (prop_name in c("base64", "data", "image_data", "content")) {
            # Use a safer way to check for slots
            if (prop_name %in% methods::slotNames(content)) {
              data_value <- methods::slot(content, prop_name)
              if (!is.null(data_value) && data_value != "") {
                return(data_value)
              }
            }
          }
          NULL
        }, error = function(e) NULL)
        
        if (!is.null(img_data) && img_data != "") {
          # If we have base64 data, use it directly
          img_src <- if (grepl("^data:image/", img_data)) {
            # Already a data URL
            img_data
          } else {
            # Assume it's raw base64 data
            paste0("data:image/png;base64,", img_data)
          }
          
          return(paste0(
            '<div class="content-image" data-type="image">',
            '<img alt="" src="', img_src, '" style="max-width:100%;display:block;margin:10px 0;">',
            '</div>'
          ))
        } else {
          return('<div class="content-image" data-type="image">(image data not found)</div>')
        }
      }
    } else if (grepl("ContentToolRequest", content_class)) {
      # Format tool request
      name <- tryCatch(content@name, error = function(e) "unknown")
      args <- tryCatch(content@arguments, error = function(e) list())
      id <- tryCatch(content@id, error = function(e) "")
      
      # Format the call
      call_text <- tryCatch({
        rlang::call2(name, !!!args) |>
          rlang::expr_deparse(width = 60) |>
          paste(collapse = "\n")
      }, error = function(e) {
        paste("Tool call:", name, "\nArguments:", 
              paste(capture.output(print(args)), collapse = "\n"))
      })
      
      return(paste0(
        '<div class="content-tool-request" data-type="tool-request" data-tool-id="', 
        html_escape(id), '" data-tool-name="', html_escape(name), '">',
        '<pre class="code-block"><code>', html_escape(call_text), '</code></pre>',
        '</div>'
      ))
    } else if (grepl("ContentToolResult", content_class)) {
      # Format tool result
      id <- tryCatch(content@id, error = function(e) "")
      error <- tryCatch(content@error, error = function(e) NULL)
      value <- tryCatch(content@value, error = function(e) "")
      
      if (is.null(error)) {
        return(paste0(
          '<div class="content-tool-result" data-type="tool-result" data-tool-id="', 
          html_escape(id), '">',
          '<pre class="tool-result"><code>', html_escape(value), '</code></pre>',
          '</div>'
        ))
      } else {
        return(paste0(
          '<div class="content-tool-error" data-type="tool-error" data-tool-id="', 
          html_escape(id), '">',
          '<pre class="tool-error"><code>', html_escape(error), '</code></pre>',
          '</div>'
        ))
      }
    } else {
      # For other S7/S4 content, try to get properties
      props <- tryCatch(S7::props(content), error = function(e) as.list(content))
      
      if (length(props) > 0) {
        # Filter out functions
        props_safe <- lapply(props, function(prop) {
          if (is.function(prop) || is.environment(prop)) return(NULL)
          prop
        })
        props_safe <- props_safe[!sapply(props_safe, is.null)]
        
        content_yaml <- tryCatch(
          yaml::as.yaml(props_safe),
          error = function(e) paste(capture.output(print(props_safe)), collapse = "\n")
        )
        
        return(paste0(
          '<div class="content-other" data-type="', content_type, '">',
          '<pre class="code-block"><code>', html_escape(content_yaml), '</code></pre>',
          '</div>'
        ))
      } else {
        return(paste0('<div class="content-empty">(empty ', content_type, ' content)</div>'))
      }
    }
  } else {
    # For unsupported content types, return a simple representation
    return(paste0(
      '<div class="content-unknown">',
      '<pre><code>', html_escape(paste("Unsupported content type:", class(content)[1])), '</code></pre>',
      '</div>'
    ))
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
  
  # Use base R for base64 encoding
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
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    svg = "image/svg+xml",
    webp = "image/webp",
    bmp = "image/bmp"
  )
  
  # Return the MIME type or default to jpeg
  return(mime_types[[ext]] %||% "image/jpeg")
}

# Simplified CSS for standalone HTML
light_theme_css <- "
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
  line-height: 1.6;
  color: #333;
  background-color: #f8f9fa;
  margin: 0;
  padding: 0;
}

.container {
  max-width: 900px;
  margin: 0 auto;
  padding: 20px;
  background-color: #ffffff;
  box-shadow: 0 0 10px rgba(0, 0, 0, 0.1);
  min-height: 100vh;
}

.chat-title {
  margin-bottom: 20px;
  padding-bottom: 10px;
  border-bottom: 1px solid #dee2e6;
  color: #495057;
}

.chat-content {
  padding: 10px 0;
}

.turn {
  margin-bottom: 20px;
  padding-bottom: 10px;
  border-bottom: 1px solid #eee;
}

.turn-content {
  margin-left: 15px;
}

.system-heading {
  color: #6610f2;
  border-bottom: 2px solid #6610f2;
  padding-bottom: 5px;
}

.user-heading {
  color: #0d6efd;
  border-bottom: 2px solid #0d6efd;
  padding-bottom: 5px;
}

.llm-heading {
  color: #198754;
  border-bottom: 2px solid #198754;
  padding-bottom: 5px;
}

pre.code-block {
  background-color: #f8f9fa;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 10px;
  overflow-x: auto;
  font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, Courier, monospace;
  font-size: 0.9em;
}

pre.tool-result {
  background-color: #e8f4f8;
  border: 1px solid #b8daff;
  border-radius: 4px;
  padding: 10px;
  overflow-x: auto;
}

pre.tool-error {
  background-color: #f8d7da;
  border: 1px solid #f5c6cb;
  border-radius: 4px;
  padding: 10px;
  overflow-x: auto;
}

.content-text p, .content-text h1, .content-text h2, .content-text h3 {
  margin-top: 0.5em;
  margin-bottom: 0.5em;
}

.content-image img {
  max-width: 100%;
  display: block;
  border-radius: 4px;
  margin: 10px 0;
}

.content-tool-request, .content-tool-result, .content-tool-error, .content-image, .content-other {
  margin: 10px 0;
}

blockquote {
  border-left: 3px solid #6c757d;
  padding-left: 15px;
  color: #6c757d;
  margin-left: 0;
}

table {
  width: 100%;
  margin-bottom: 1rem;
  border-collapse: collapse;
}

table th,
table td {
  padding: 0.5rem;
  border: 1px solid #dee2e6;
}

table th {
  background-color: #f8f9fa;
}

code {
  font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, Courier, monospace;
  background-color: #f8f9fa;
  padding: 2px 4px;
  border-radius: 3px;
  font-size: 0.9em;
}
"

dark_theme_css <- "
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Open Sans', 'Helvetica Neue', sans-serif;
  line-height: 1.6;
  color: #f8f9fa;
  background-color: #222;
  margin: 0;
  padding: 0;
}

.container {
  max-width: 900px;
  margin: 0 auto;
  padding: 20px;
  background-color: #2c3034;
  box-shadow: 0 0 10px rgba(0, 0, 0, 0.3);
  min-height: 100vh;
}

.chat-title {
  margin-bottom: 20px;
  padding-bottom: 10px;
  border-bottom: 1px solid #495057;
  color: #adb5bd;
}

.chat-content {
  padding: 10px 0;
}

.turn {
  margin-bottom: 20px;
  padding-bottom: 10px;
  border-bottom: 1px solid #343a40;
}

.turn-content {
  margin-left: 15px;
}

.system-heading {
  color: #a78bfa;
  border-bottom: 2px solid #a78bfa;
  padding-bottom: 5px;
}

.user-heading {
  color: #60a5fa;
  border-bottom: 2px solid #60a5fa;
  padding-bottom: 5px;
}

.llm-heading {
  color: #34d399;
  border-bottom: 2px solid #34d399;
  padding-bottom: 5px;
}

pre.code-block {
  background-color: #2c3034;
  border: 1px solid #495057;
  border-radius: 4px;
  padding: 10px;
  overflow-x: auto;
  font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, Courier, monospace;
  font-size: 0.9em;
}

pre.tool-result {
  background-color: #1e3a5f;
  border: 1px solid #0d6efd;
  border-radius: 4px;
  padding: 10px;
  overflow-x: auto;
}

pre.tool-error {
  background-color: #541e3a;
  border: 1px solid #dc3545;
  border-radius: 4px;
  padding: 10px;
  overflow-x: auto;
}

.content-text p, .content-text h1, .content-text h2, .content-text h3 {
  margin-top: 0.5em;
  margin-bottom: 0.5em;
}

.content-image img {
  max-width: 100%;
  display: block;
  border-radius: 4px;
  margin: 10px 0;
}

.content-tool-request, .content-tool-result, .content-tool-error, .content-image, .content-other {
  margin: 10px 0;
}

blockquote {
  border-left: 3px solid #adb5bd;
  padding-left: 15px;
  color: #adb5bd;
  margin-left: 0;
}

table {
  width: 100%;
  margin-bottom: 1rem;
  border-collapse: collapse;
}

table th,
table td {
  padding: 0.5rem;
  border: 1px solid #495057;
}

table th {
  background-color: #343a40;
}

code {
  font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, Courier, monospace;
  background-color: #343a40;
  padding: 2px 4px;
  border-radius: 3px;
  font-size: 0.9em;
}
"