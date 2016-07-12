

# m2 symbol name character
m2_symbol_chars <- function() {
  unlist(
    c(letters, toupper(letters), seq(10)-1, c("'"))
  )
}

# m2 operators, sorted by length for easier tokenizing
m2_operators <- function() {
  c(
    "===>", "<==>", "<===",
    "==>", "===", "=!=", "<==", "^**", "(*)", "..<",
    "||", "|-", ">>", ">=", "=>", "==", "<=", "<<", "<-", "++", "^^",
    "^*", "#?", "//", "**", "@@", "..", ".?", "!=", ":=", "->", "_*",
    "~", "|", ">", "=", "<", "+", "^", "%", "#", "&", "\\", "/", "*",
    "@", ".", "?", "!", ":", ";", ",", "-", "_",
    "[", "]", "{", "}"
  )
}

# splits a string containing M2 code into tokens to ease parsing
# places an empty string between each line
m2_tokenize <- function(s) {
  # operatorchars <- unlist(strsplit("=<>!&|_^{}[]()+-*/\\:;.,?`~@#$", "", fixed = TRUE))
  operatorstartchars <- unlist(lapply(m2_operators(), function(s) substr(s,1,1)))

  tokens <- character()

  i <- 1
  while (i <= nchar(s)) {
    curchar <- substr(s, i, i)
    if (curchar %in% m2_symbol_chars()) {

      start <- i
      i <- i + 1
      while (i <= nchar(s) && substr(s, i, i) %in% m2_symbol_chars()) {
        i <- i + 1
      }
      i <- i - 1
      end <- i

      tokens <- append(tokens, substr(s,start,end))

    } else if (curchar %in% operatorstartchars) {

      # substr() is smart enough to not index past the end of the string
      for (op in m2_operators()) {
        if (op == substr(s, i, i + nchar(op) - 1)) {
          tokens <- append(tokens, op)
          i <- i + nchar(op) - 1
          break()
        }
      }

    } else if (curchar == "\"") {

      i <- i + 1

      start <- i
      while (i <= nchar(s) && substr(s, i, i) != "\"") {
        if (substr(s, i, i) == "\\") i <- i + 1
        i <- i + 1
      }
      end <- i - 1

      tokens <- append(tokens, c("\"", substr(s,start,end), "\""))

    } else if (curchar == "\n") {

      tokens <- append(tokens, "")

    }

    # skip other whitespace, etc.
    i <- i + 1
  }

  tokens
}


m2_parse <- function(tokens, start = 1, retnextindex = FALSE) {

  i <- start

  if (tokens[i] == "{") {
    # list: {A, A2 => B2, A3 => B3, C, ...}

    elem <- m2_parse_list(tokens, start = i, retnextindex = TRUE)
    ret <- elem$result
    i <- elem$nIndex

  } else if (tokens[i] == "[") {
    # array: [A, B, ...]

    elem <- m2_parse_array(tokens, start = i, retnextindex = TRUE)
    ret <- elem$result
    i <- elem$nIndex

  } else if (tokens[i] == "\"") {
    # string: "stuff"

    error_on_fail(tokens[i+2] == "\"", "Parsing error: malformed string.")
    ret <- tokens[i+1]
    i <- i + 3

  } else if (substr(tokens[i], 1, 1) %in% (seq(10)-1)) {
    # number

    ret <- strtoi(tokens[i])
    i <- i + 1

  } else if (tokens[i] == "new") {
    # object creation: new TYPENAME from DATA

    elem <- m2_parse_new_object(tokens, start = i, retnextindex = TRUE)
    ret <- elem$result
    i <- elem$nIndex

  } else if (tokens[i] == "symbol") {
    # symbol name

    ret <- tokens[i+1]
    class(ret) <- c("M2","Symbol")
    i <- i + 2

  } else if (substr(tokens[i], 1, 1) %in% m2_symbol_chars()) {
    # symbol, must be final case
    # TODO: implement wrapper support (where possible)

    ret <- tokens[i]
    i <- i + 1

    if (ret == "true") {
      ret <- TRUE
    } else if (ret == "false") {
      ret <- FALSE
    } else if (ret == "null") {
      ret <- NULL
    } else {
      class(ret) <- c("M2","Symbol")
    }

  } else {
    # we can't handle this input

    stop(paste("Parsing error: format not supported: ", tokens[i]))

  }

  if (i <= length(tokens) && tokens[i] == "=>") {
    # option: A => B

    ret1 <- ret

    elem <- m2_parse(tokens, start = i+1, retnextindex = TRUE)
    ret2 <- elem$result
    i <- elem$nIndex

    ret <- list(ret1, ret2)
    class(ret) <- c("M2","Option")
  }

  return(parse_return(retnextindex, ret, i))

}

m2_parse_new_object <- function(tokens, start = 1, retnextindex = FALSE) {

  i <- start + 1

  error_on_fail(tokens[i-1] == "new", "Parsing error: malformed new object")

  if (tokens[i] %in% c("OptionTable", "HashTable", "MutableHashTable", "VerticalList")) {

    error_on_fail(tokens[i+1] == "from", "Parsing error: malformed OptionTable.")

    elem <- m2_parse(tokens, start = i+2, retnextindex = TRUE)
    ret <- elem$result
    class(ret) <- c("M2",tokens[i])
    i <- elem$nIndex

  } else {

    stop(paste(tokens[i], "not yet supported"))

  }

  return(parse_return(retnextindex, ret, i))

}

# [A, B, ...]
m2_parse_array <- function(tokens, start = 1, retnextindex = FALSE) {

  ret <- list()
  i <- start + 1

  error_on_fail(tokens[i-1] == "[", "Parsing error: malformed array.")

  if (tokens[i] == "]") {
    i <- i + 1
  } else {
    while (TRUE) {

      elem <- m2_parse(tokens, start = i, retnextindex = TRUE)
      ret <- append(ret, elem$result)
      i <- elem$nIndex + 1

      if (tokens[i-1] == "]") {
        break()
      }

      error_on_fail(tokens[i-1] == ",", "Parsing error: malformed array")
      error_on_fail(i <= length(tokens), "Parsing error: malformed array")

    }
  }

  class(ret) <- c("M2","Array")

  return(parse_return(retnextindex, ret, i))

}

# {A1 => B1, A2 => B2, ...}
m2_parse_list <- function(tokens, start = 1, retnextindex = FALSE) {

  ret <- list()
  i <- start + 1

  error_on_fail(tokens[i-1] == "{", "Parsing error: malformed list")

  if (tokens[i] == "}") {
    i <- i + 1
  } else {
    while (TRUE) {

      elem <- m2_parse(tokens, start = i, retnextindex = TRUE)

      ret <- append(ret, elem$result);
      i <- elem$nIndex + 1

      if (tokens[i-1] == "}") {
        break()
      }

      error_on_fail(tokens[i-1] == ",", "Parsing error: malformed list")
      error_on_fail(i <= length(tokens), "Parsing error: malformed list")

    }
  }

  class(ret) <- c("M2","List")

  return(parse_return(retnextindex, ret, i))

}


error_on_fail <- function(t, e) {
  if (!t) {
    stop(e)
  }
}

parse_return <- function(retnextindex, ret, i) {

  if (retnextindex) {
    return(list(result = ret, nIndex = i))
  } else {
    return(ret)
  }

}



