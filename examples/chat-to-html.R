# Example: Converting an ellmer Chat to self-contained HTML
#
# This is an executable example that demonstrates how to convert
# an ellmer Chat object to a self-contained HTML file.

# Load required libraries
# library(ellmer)
# library(promptdown)

# Step 1: Create a chat session with any model
# For example, using a local Ollama model:
# chat <- ellmer::chat_ollama(
#   system_prompt = "You are a helpful assistant specialized in explaining concepts clearly.",
#   model = "llama3"  # use a model you have locally
# )

# Step 2: Have a conversation
# chat$chat("What's the difference between a meteor, meteoroid, and meteorite?")
# chat$chat("Can you give an example of a famous meteorite?")

# Step 3: Export the conversation to a self-contained HTML file
# html_path <- file.path(tempdir(), "chat_export.html")
# write_chat_html(chat, html_path, theme = "light", embed_images = TRUE)

# Step 4: Open the HTML file in a web browser
# browseURL(html_path)

# Note: You can also get the HTML as a string without writing to a file
# html_string <- write_chat_html(chat)
# cat("Generated HTML length:", nchar(html_string), "characters\n")

# Different themes are available:
# dark_html_path <- file.path(tempdir(), "chat_export_dark.html")
# write_chat_html(chat, dark_html_path, theme = "dark", embed_images = TRUE)
# browseURL(dark_html_path)