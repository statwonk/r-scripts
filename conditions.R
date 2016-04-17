# Source: http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling

condition <- function(subclass, message,
                      # "This is shown here with sys.calls(), which is the run-time equivalent of traceback() —
                      # it lists all calls leading to the current function."

                      # "sys.call, sys.frame and sys.function accept integer values for the argument
                      #  which. Non-negative values of which are frame numbers whereas negative
                      #  values are counted back from the frame number of the current evaluation."
                      call = sys.call(-1),
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
  condition(c("malformed_log_entry", "error"),
            message = msg,
            text = text
  )
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
  return(paste("Successfully parsed:", text))
}

parse_log_entry("malformed entry")
parse_log_entry("parsable entry")

parse_log_entries <- function(...) {
  lapply(list(...), function(text) {
    tryCatch(
      parse_log_entry(text),
      skip_log_entry = function(e) NULL,
      malformed_log_entry = function(e) paste("Not parsed", e$text)
    )
  })
}

parse_log_entries("parsable entry",
                  "another parsable entry",
                  "malformed entry")
