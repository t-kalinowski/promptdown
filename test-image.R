library(devtools)
load_all()

# Create a test HTML with an image
test_dir <- tempdir()
test_image_path <- file.path(test_dir, "test_image.png")

# Create a simple 1x1 transparent PNG
pixel_base64 <- "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
pixel_raw <- base64enc::base64decode(pixel_base64)
writeBin(pixel_raw, test_image_path)

# Create a simple test chat
test_chat <- list(
  list(
    role = "user",
    contents = list(
      list(text = "Here's a test image:")
    )
  ),
  list(
    role = "user",
    contents = list(
      list(path = test_image_path)
    )
  ),
  list(
    role = "assistant",
    contents = list(
      list(text = "I can see your test image.")
    )
  )
)

# Generate an HTML file
test_html_path <- file.path(test_dir, "test_chat.html")
write_chat_html(test_chat, test_html_path, embed_images = TRUE)

# Print paths for reference
cat("Image saved at:", test_image_path, "\n")
cat("HTML saved at:", test_html_path, "\n")

# Optional: open in browser
cat("To view the HTML, run: browseURL('", test_html_path, "')\n", sep="")
