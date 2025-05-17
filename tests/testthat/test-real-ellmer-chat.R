test_that("write_chat_html works with real ellmer::Chat objects", {
  # Skip if ellmer not available
  skip_if_not_installed("ellmer")
  skip_on_ci() # Skip in CI environments
  skip_if_not(interactive(), "Test requires interactive session")
  
  # For interactive testing with a real ellmer chat:
  # library(ellmer)
  # library(promptdown)
  #
  # # Create a chat instance
  # chat <- ellmer::chat_ollama(system_prompt = "be flat", model = "gemma3:1b-it-qat")
  # chat$chat("hello")
  #
  # # Write the chat to HTML
  # html_path <- file.path(tempdir(), "test_chat.html")
  # write_chat_html(chat, html_path)
  # browseURL(html_path)
})