#' Convert a M2 object into an R object
#'
#' Convert a M2 object into an R object
#'
#' @param x x
#' @param ... ...
#' @return an R object
#' @name m2_to_r
#' @examples
#'
#' \dontrun{ requires Macaulay2 be installed
#'
#'
#'
#' }








#' @rdname m2_to_r
#' @export
m2_to_r <- function(x, ...) UseMethod("m2_to_r")





m2_to_r.default <- function(x, ...) x$ext_str











# m2 symbol name character
m2_symbol_chars <- function() {
  c(letters, toupper(letters), 0:9, "'")
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
    "[", "]", "{", "}", "(", ")"
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
          break
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












m2_parse <- function(tokens, start = 1) {
  ret <- m2_parse_internal(tokens, start)
  ret$result
}



m2_parse_internal <- function(tokens, start = 1) {

  i <- start

  if (tokens[i] == "{") {
    # list: {A, A2 => B2, A3 => B3, C, ...}

    elem <- m2_parse_list(tokens, start = i)
    ret <- elem$result
    i <- elem$nIndex

  } else if (tokens[i] == "[") {
    # array: [A, B, ...]

    elem <- m2_parse_array(tokens, start = i)
    ret <- elem$result
    i <- elem$nIndex

  } else if (tokens[i] == "(") {
    # sequence: (A, B, ...) returned as classed list OR (A) returned as A

    elem <- m2_parse_sequence(tokens, start = i)
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

  } else if (tokens[i] == "-") {
    # -expression

    elem <- -m2_parse_internal(tokens,start = i+1)
    ret <- -elem$result
    i <- elem$nIndex

  } else if (tokens[i] == "new") {
    # object creation: new TYPENAME from DATA

    elem <- m2_parse_new(tokens, start = i)
    ret <- elem$result
    i <- elem$nIndex

  } else if (substr(tokens[i], 1, 1) %in% m2_symbol_chars()) {
    # symbol, must be final case handled

    elem <- m2_parse_symbol(tokens, start = i)
    ret <- elem$result
    i <- elem$nIndex

  } else {
    # we can't handle this input

    stop(paste("Parsing error: format not supported: ", tokens[i]))

  }

  if (i > length(tokens)) {
    return(list(result = ret, nIndex = i))
  }

  if (tokens[i] == "=>") {
    # option: A => B

    key <- ret

    elem <- m2_parse_internal(tokens, start = i+1)
    val <- elem$result
    i <- elem$nIndex

    ret <- list(key, val)
    class(ret) <- c("m2_option","m2")

  } else if (tokens[i] == "..") {
    # sequence: (a..c) = (a, b, c)

    start <- ret

    elem <- m2_parse_internal(tokens, start = i+1)
    end <- elem$result
    i <- elem$nIndex

    if (all(c(start,end) %in% letters) && start <= end) {
      ret <- as.list(start %:% end)
      ret <- lapply(ret, `class<-`, c("m2_symbol","m2"))
    } else if (all(c(start,end) %in% toupper(letters)) && start <= end) {
      ret <- as.list(start %:% end)
      ret <- lapply(ret, `class<-`, c("m2_symbol","m2"))
    } else if (is.integer(start) && is.integer(end) && start <= end) {
      ret <- as.list(start:end)
    } else {
      ret <- list()
    }

    class(ret) <- c("m2_sequence","m2")

  } else if (class(ret)[1] %in% c("m2_ring","m2_symbol") &&
             (tokens[i] %notin% c(m2_operators(),",") ||
              tokens[i] %in% c("(","{","["))) {
    # function call

    elem <- m2_parse_internal(tokens, start = i)
    params <- elem$result
    i <- elem$nIndex

    if (class(ret)[1] == "m2_symbol") {
      fname <- ret
      params = list(params)
      class(params) <- c(paste0("m2_",tolower(fname)),"m2")

      ret <- m2_parse_function(params)
    } else {
      # this is a ring, create

    }

  } else if (tokens[i] %in% c("+","-","*","^")) {
    # start of an expression, consume rest of expression
    # TODO: parse mpoly here!

    lhs <- ret
    operand <- tokens[i]

    elem <- m2_parse_internal(tokens, start = i + 1)
    rhs <- elem$result
    i <- elem$nIndex

    ret <- paste0(lhs, operand, rhs)

    if ((is.integer(lhs) || class(lhs)[1] %in% c("m2_expression", "m2_symbol")) &&
        (is.integer(rhs) || class(rhs)[1] %in% c("m2_expression", "m2_symbol"))) {
      class(ret) <- c("m2_expression", "m2")
    }

  }

  list(result = ret, nIndex = i)

}







# x is a list interpreted as a M2 list
# class name is m2_M2CLASSNAME in all lower case
# example: x = list(1,2,3), class(x) = c("m2_verticallist","m2")
m2_parse_class <- function(x) UseMethod("m2_parse_class")

m2_parse_class.default <- function(x) x

m2_parse_class.m2_hashtable <- m2_parse_class.default
m2_parse_class.m2_optiontable <- m2_parse_class.default
m2_parse_class.m2_verticallist <- m2_parse_class.default





# x is a list of function parameters
# class name is m2_M2FUNCTIONNAME in all lower case
# example: x = list(mpoly("x")), class(x) = c("m2_symbol","m2")
m2_parse_function <- function(x) UseMethod("m2_parse_function")
m2_parse_function.default <- function(x) stop(paste0("Unsupported function ", class(x)[1]))


m2_parse_function.m2_symbol <- function(x) {

  class(x[[1]]) <- c("m2_symbol","m2")
  x[[1]]

}


m2_parse_function.m2_monoid <- function(x) {

  class(x[[1]]) <- c("m2_monoid","m2")
  x[[1]]

}



m2_parse_function.m2_hashtable <- function(x) {
  x[[1]]
}

m2_parse_function.m2_optiontable <- m2_parse_function.m2_hashtable
m2_parse_function.m2_verticallist <- m2_parse_function.m2_hashtable




m2_parse_object_as_function <- function(x) UseMethod("m2_parse_object_as_function")
m2_parse_object_as_function.default <- function(x) stop(paste0("Unsupported object ", class(x)[1], " used as function"))

m2_parse_object_as_function.m2_ring <- function(x) {

}




m2_parse_new <- function(tokens, start = 1) {

  i <- start

  error_on_fail(tokens[i] == "new", "Parsing error: malformed new object")
  error_on_fail(tokens[i+2] == "from", "Parsing error: malformed new object")

  elem <- m2_parse_internal(tokens, start = i+3)
  ret <- elem$result
  i <- elem$nIndex

  class(ret) <- c(paste0("m2_",tolower(tokens[start+1])),"m2")

  m2_parse_class(ret)

  list(result = ret, nIndex = i)

}




m2_parse_symbol <- function(tokens, start = 1) {

  i <- start + 1
  ret <- tokens[i-1]

  # TODO: handle ring case here

  while (i <= length(tokens) && tokens[i] == "_") {
    ret <- paste0(ret,"_",tokens[i+1])
    i <- i + 2
  }

  if (ret == "true") {
    ret <- TRUE
  } else if (ret == "false") {
    ret <- FALSE
  } else if (ret == "null") {
    ret <- NULL
  } else {
    # this is an actual symbol
    class(ret) <- c("m2_symbol","m2")
  }

  list(result = ret, nIndex = i)

}



# {A1 => B1, A2 => B2, ...}
m2_parse_list <- function(tokens, start = 1, open_char = "{", close_char = "}", type_name = "list") {

  ret <- list()
  i <- start + 1

  error_on_fail(tokens[i-1] == open_char, paste0("Parsing error: malformed ", type_name))

  if (tokens[i] == close_char) {
    i <- i + 1
  } else {
    repeat {

      elem <- m2_parse_internal(tokens, start = i)
      ret <- append(ret, list(elem$result))
      i <- elem$nIndex + 1

      if (tokens[i-1] == close_char) {
        break()
      }

      error_on_fail(tokens[i-1] == ",", paste0("Parsing error: malformed ", type_name))
      error_on_fail(i <= length(tokens), paste0("Parsing error: malformed ", type_name))

    }
  }

  class(ret) <- c(paste0("m2_",type_name),"m2")

  list(result = ret, nIndex = i)

}







# [A, B, ...]
m2_parse_array <- function(tokens, start = 1) {

  m2_parse_list(tokens, start = start, open_char = "[", close_char = "]", type_name = "array")

}









# (A, B, ...) as classed list
# (A1) as A1
m2_parse_sequence <- function(tokens, start = 1) {

  elem <- m2_parse_list(tokens, start = start, open_char = "(", close_char = ")", type_name = "sequence")

  # if sequence has only one element
  if (length(elem) == 1) {
    elem$result <- elem$result[1]
  }

  elem

}







error_on_fail <- function(t, e) {
  if (!t) stop(e)
}






peek <- function(tokens, i) {
  if (i > length(tokens)) {
    return(NULL)
  } else {
    return(tokens[i])
  }
}


















