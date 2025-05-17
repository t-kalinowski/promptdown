test_that("write_chat_html works with ellmer::Chat object", {
  # Skip if ellmer not available
  skip_if_not_installed("ellmer")
  
  # Skip this test in non-interactive mode
  skip_if_not(interactive(), "Test requires interactive session")
  
  # For interactive testing with a real ellmer chat:
  # chat <- ellmer::chat_ollama(system_prompt = "be flat", model = "gemma3:1b-it-qat")
  # chat$chat("hello")
  # html_path <- file.path(tempdir(), "test_chat.html")
  # write_chat_html(chat, html_path)
  # browseURL(html_path)
})

test_that("write_chat_html works with a simple list of messages", {
  # Create a simple list of messages with role and content
  simple_messages <- list(
    list(
      role = "system",
      contents = list(list(text = "You are a helpful assistant."))
    ),
    list(
      role = "user",
      contents = list(list(text = "Hello, how are you?"))
    ),
    list(
      role = "assistant",
      contents = list(list(text = "I'm doing well, thank you for asking!"))
    )
  )
  
  # Call write_chat_html with the messages list directly
  temp_file <- tempfile(fileext = ".html")
  on.exit(unlink(temp_file))
  
  html <- write_chat_html(simple_messages, temp_file)
  
  # Check that HTML was written
  expect_true(file.exists(temp_file))
  expect_true(file.size(temp_file) > 0)
  
  # Read the HTML and check it contains expected content
  html_content <- readLines(temp_file, warn = FALSE)
  html_content_str <- paste(html_content, collapse = "\n")
  
  expect_match(html_content_str, "You are a helpful assistant", fixed = TRUE)
  expect_match(html_content_str, "Hello, how are you?", fixed = TRUE)
  expect_match(html_content_str, "I'm doing well, thank you for asking!", fixed = TRUE)
})

test_that("write_chat_html handles image embedding", {
  # Skip if base64enc not available
  skip_if_not_installed("base64enc")
  
  # Create a temporary PNG image 
  temp_img <- tempfile(fileext = ".png")
  on.exit(unlink(temp_img))
  
  # Create a simple PNG - we use a base64-encoded 1x1 transparent pixel
  # This is just for testing - in real usage, it would be a real image file
  pixel_base64 <- "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
  pixel_raw <- base64enc::base64decode(pixel_base64)
  writeBin(pixel_raw, temp_img)
  
  # Create a simple list of messages including an image
  messages_with_image <- list(
    list(
      role = "user",
      contents = list(list(text = "Here's an image:"))
    ),
    list(
      role = "user",
      contents = list(list(path = temp_img))
    ),
    list(
      role = "assistant",
      contents = list(list(text = "I see the image."))
    )
  )
  
  # Test with embedding = TRUE
  temp_file_embedded <- tempfile(fileext = ".html")
  on.exit(unlink(temp_file_embedded))
  
  html_embedded <- write_chat_html(messages_with_image, temp_file_embedded, embed_images = TRUE)
  
  # Read the HTML and check it contains the base64 data
  html_content <- readLines(temp_file_embedded, warn = FALSE)
  html_content_str <- paste(html_content, collapse = "\n")
  
  # We expect image data to be embedded if the file exists
  if (file.exists(temp_img)) {
    expect_match(html_content_str, "data:image/png;base64,", fixed = TRUE)
  }
  
  # Test with embedding = FALSE
  temp_file_linked <- tempfile(fileext = ".html")
  on.exit(unlink(temp_file_linked))
  
  html_linked <- write_chat_html(messages_with_image, temp_file_linked, embed_images = FALSE)
  
  # Read the HTML and check it contains the file path
  html_content_linked <- readLines(temp_file_linked, warn = FALSE)
  html_content_linked_str <- paste(html_content_linked, collapse = "\n")
  
  expect_match(html_content_linked_str, temp_img, fixed = TRUE)
  expect_false(grepl("data:image/png;base64,", html_content_linked_str, fixed = TRUE))
})
EOF < /dev/null