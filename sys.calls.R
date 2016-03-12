# Source: http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling

# f -> g -> h()

f <- function() g()
g <- function() h()
h <- function() stop("!")

# Below sys.calls(-1) yields the clearest error,
# this is why Hadley's using sys.call(-1), I believe
# in conditions.R.
tryCatch(f(), error = function(e) print(sys.calls()))
