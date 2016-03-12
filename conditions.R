condition <- function(subclass, message,
                      call = sys.call(-2), # -2 ?? -3 seems to produce a clearer error
                      ...) {
  # Called inside "error catching" function, condition captures
  # context of the caller and attach the caller's frame as an
  # attribute.
  structure(
    class = c(subclass, "condition"), # condition is a metaclass
    list(message = message, call = call, ...)
  )
}

malformed_log_entry_error <- function(text) {
  # Error handling function. Returns a condition.
  msg <- paste0("Malformed log entry: ", text)
  condition(c("malformed_log_entry_entry", "error"),
            message = msg,
            text = text
  )

  # Is this where we control recovery behavior?
}

malformed_log_entry <- function(text) {
  if(grepl("malformed entry", text)) {
    TRUE
  } else {
    FALSE
  }
}

parse_log_entry <- function(text) {
  if (malformed_log_entry(text)) {
    stop(malformed_log_entry_error(text))
  }
  return()
}

parse_log_entry("malformed entry")
