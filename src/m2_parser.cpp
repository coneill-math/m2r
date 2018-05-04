#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//


// [[Rcpp::export]]
std::vector<std::string> m2_symbol_chars_cpp() {
  // c(letters, toupper(letters), 0:9, "'")
  const char *symbolchars[] = {
    "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
    "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",
    "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
    "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
    "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "'"
  };
  return std::vector<std::string>(symbolchars, symbolchars + sizeof(symbolchars)/sizeof(const char *));
}

// [[Rcpp::export]]
std::vector<std::string> m2_operators_cpp() {
  // m2 operators, sorted by length for easier tokenizing
  const char *operators[] = {
    "===>", "<==>", "<===",
    "==>", "===", "=!=", "<==", "^**", "(*)", "..<",
    "||", "|-", ">>", ">=", "=>", "==", "<=", "<<", "<-", "++", "^^",
    "^*", "#?", "//", "**", "@@", "..", ".?", "!=", ":=", "->", "_*",
    "~", "|", ">", "=", "<", "+", "^", "%", "#", "&", "\\", "/", "*",
    "@", ".", "?", "!", ":", ";", ",", "-", "_",
    "[", "]", "{", "}", "(", ")"
  };
  return std::vector<std::string>(operators, operators + sizeof(operators)/sizeof(const char *));
}


// splits a string containing M2 code into tokens to ease parsing
// places an empty string between each line
// [[Rcpp::export]]
std::vector<std::string> m2_tokenize_cpp(std::string &s) {
  std::vector<std::string> m2operators = m2_operators_cpp();
  std::vector<std::string> m2symbolchars = m2_symbol_chars_cpp();
  std::vector<std::string> tokens;

  std::vector<std::string> operatorstartchars(m2operators.size());
  for(size_t i = 0;i < m2operators.size();i++) {
    operatorstartchars[i] = m2operators[i][0];
  }

  for(size_t i = 0;i < s.length();) {
    if (s[i] == ' ' || s[i] == '\t') {
      i++;
    } else if (std::find(m2symbolchars.begin(), m2symbolchars.end(), std::string(&s[i], 1)) != m2symbolchars.end()) {
      size_t start = i;

      i++;
      while (i < s.length() && std::find(m2symbolchars.begin(), m2symbolchars.end(), std::string(&s[i], 1)) != m2symbolchars.end()) {
        i++;
      }

      tokens.push_back(s.substr(start, i - start));
    } else if (std::find(operatorstartchars.begin(), operatorstartchars.end(), std::string(&s[i], 1)) != operatorstartchars.end()) {
      // substr() is smart enough to not index past the end of the string
      for (size_t j = 0;j < m2operators.size();j++) {
        std::string op = m2operators[j];
        if (i + op.length() <= s.length() && op == s.substr(i, op.length())) {
          tokens.push_back(op);
          i = i + op.length();
          break;
        }
      }
    } else if (s[i] == '"') {
      i++;
      size_t start = i;
      for(;i < s.length() && s[i] != '"';i++) {
        if (s[i] == '\\')
          i++;
      }

      tokens.push_back("\"");
      tokens.push_back(s.substr(start, i - start));
      tokens.push_back("\"");
      i++;
    } else if (s[i] == '\n') {
      tokens.push_back("");
    }
    else
      i++;
  }

  return tokens;
}























// return List::create(Rcpp::Named("a") = "529a", Rcpp::Named("b") = s);







/*

#' @rdname m2_parser
#' @export
m2_parse <- function(s) {

  if (is.m2_pointer(s)) {
    tokens <- m2_tokenize(m2_meta(s, "ext_str"))
  } else if (is.m2(s)) {
    return(s)
  } else {
    tokens <- m2_tokenize(s)
  }

  memoise::forget(mem_m2.)
    memoise::forget(mem_m2_parse)

    ret <- m2_parse_internal(tokens)
    ret <- ret$result

    if (
        is.m2_pointer(s) && is.m2(ret) &&
          !is.null(m2_name(ret)) && m2_name(ret) == ""
    ) {
      m2_name(ret) <- m2_name(s)
    }

    memoise::forget(mem_m2.)
      memoise::forget(mem_m2_parse)

      ret

}












# only used for ring parsing!  Don't get greedy!!!!
mem_m2. <- memoise::memoise(function(x) m2.(x))
  mem_m2_parse <- memoise::memoise(function(x) m2_parse(x))


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
      ret <- m2_structure(tokens[i+1], m2_class = "m2_string")
      i <- i + 3

    } else if (substr(tokens[i], 1, 1) %in% 0:9) {
# positive integer or rational

      if (length(tokens) > i && str_detect(tokens[i+1], "/")) { # is fraction

        if (get_m2_gmp()) {
          ret <- m2_structure(
              as.bigq(tokens[i], tokens[i+2]),
              m2_class = "m2_rational"
          )
          class(ret) <- c(class(ret), "bigq")
          i <- i + 3
        } else {
          ret <- m2_structure(
              as.integer(tokens[i]) / as.integer(tokens[i+2]),
              m2_class = "m2_float"
          )
          i <- i + 3
        }

      } else { # positive integer

        if (get_m2_gmp()) {
          ret <- m2_structure(
              as.bigz(tokens[i]),
              m2_class = "m2_integer"
          )
          class(ret) <- c(class(ret), "bigz")
          i <- i + 1
        } else {
          ret <- m2_structure(
              as.integer(tokens[i]),
              m2_class = "m2_integer"
          )
          i <- i + 1
        }

      }

    } else if (tokens[i] == ".") {
# positive float

      ret <- m2_structure(
          as.double(paste0(".", str_replace(tokens[i+1], "p[0-9]+", ""))),
          m2_class = "m2_float"
      )
      i <- i + 2

    } else if (tokens[i] == "-" && i != length(tokens) && tokens[i+1] == ".") {
# negative float

      ret <- m2_structure(
          -as.double(paste0(".", str_replace(tokens[i+2], "p[0-9]+", ""))),
          m2_class = "m2_float"
      )
      i <- i + 3

    } else if (tokens[i] == "-") {
# -expression

      elem <- m2_parse_internal(tokens,start = i+1)
      ret <- elem$result
      i <- elem$nIndex

      if (is.integer(ret)) {
        ret <- -ret
      } else {
        ret <- m2_structure(paste0("-", ret), m2_class = "m2_string")
      }

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

    } else if (tokens[i] == ":") {
# sequence: (n:x) = (x,...,x)

      num_copies <- ret

      elem <- m2_parse_internal(tokens, start = i+1)
      item <- elem$result
      i <- elem$nIndex

      ret <- replicate(num_copies, item, simplify = FALSE)
      class(ret) <- c("m2_sequence","m2")

    } else if (#class(ret)[1] %in% c("m2_ring","m2_symbol") &&
      (tokens[i] %notin% c(m2_operators(),",") ||
      tokens[i] %in% c("(","{","["))) {
# function call

      if (tokens[i] == "(") {
        elem <- m2_parse_sequence(tokens, start = i, save_paren = TRUE)
      } else {
        elem <- m2_parse_internal(tokens, start = i)
        elem$result <- list(elem$result)
      }

      params <- elem$result
        i <- elem$nIndex

        ret <- m2_parse_object_as_function(ret, params)

    } else if (tokens[i] %in% c("+","-","*","^")) {
# start of an expression, consume rest of expression

      lhs <- ret
      operand <- tokens[i]

      elem <- m2_parse_internal(tokens, start = i + 1)
      rhs <- elem$result
      i <- elem$nIndex

      if (is.m2_polynomialring(lhs)) {
        ret <- list(lhs, rhs)
        class(ret) <- c("m2_module","m2")
      } else {
        ret <- paste0(lhs, operand, rhs)

        if ((is.integer(lhs) || class(lhs)[1] %in% c("m2_expression", "m2_symbol")) &&
            (is.integer(rhs) || class(rhs)[1] %in% c("m2_expression", "m2_symbol"))) {
          class(ret) <- c("m2_expression", "m2")
        }
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
  m2_parse_class.m2_optiontable <- m2_parse_class.m2_hashtable
  m2_parse_class.m2_verticallist <- m2_parse_class.m2_hashtable





# x is a list of function parameters
# class name is m2_M2FUNCTIONNAME in all lower case
# example: x = list(mpoly("x")), class(x) = c("m2_symbol","m2")
  m2_parse_function <- function(x) UseMethod("m2_parse_function")
    m2_parse_function.default <- function(x) stop(paste0("Unsupported function ", class(x)[1]))

    m2_parse_function.m2_hashtable <- function(x) x[[1]]
  m2_parse_function.m2_optiontable <- m2_parse_function.m2_hashtable
    m2_parse_function.m2_verticallist <- m2_parse_function.m2_hashtable


    m2_parse_function.m2_symbol <- function(x) {

      class(x[[1]]) <- c("m2_symbol","m2")
      x[[1]]

    }


  m2_parse_function.m2_monoid <- function(x) {

    class(x[[1]]) <- c("m2_monoid","m2")
    x[[1]]

  }


  m2_parse_function.m2_tocc <- function(x) {
    m2_structure(complex(real = x[[1]], imaginary = x[[2]]), m2_class = "m2_complex")
  }


# x is an object being applied (as a function) to params
# example: x = monoid, params = [x,y,z]
# example: x = QQ, params = monoid [x..z]
  m2_parse_object_as_function <- function(x, params) UseMethod("m2_parse_object_as_function")
    m2_parse_object_as_function.default <- function(x, params) stop(paste0("Unsupported object ", class(x)[1], " used as function"))


# x is a function name
# dispatch for function call
    m2_parse_object_as_function.m2_symbol <- function(x, params) {

      class(params) <- c(paste0("m2_",tolower(x)),"m2")

      ret <- m2_parse_function(params)

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
      sym_name <- tokens[i-1]

      ptr <- mem_m2.(sym_name)

      if (m2_meta(ptr, "m2_class") %in% m2_ring_class_names()) {

        ret <- ""
        if (sym_name %in% m2_coefrings()) {
          ret <- coefring_as_ring(sym_name)
        } else {
          ret <- mem_m2_parse(ptr)
          m2_name(ret) <- sym_name
        }

        while (i <= length(tokens) && tokens[i] == "_") i <- i + 2

        return(list(result = ret, nIndex = i))

      }

      ret <- sym_name
        while (i <= length(tokens) && tokens[i] == "_") {
          i <- i + 1

          if (tokens[i] == "(") {
            seqret <- m2_parse_sequence(tokens, start = i)
            ret <- paste0(
                ret,
                paste0(unlist(tokens[i:seqret$nIndex-1]), collapse = "")
            )
            i <- seqret$nIndex
          } else {
            ret <- paste0(ret,"_",tokens[i])
          }

          i <- i + 1

        }

        if (ret == "true") {
          ret <- m2_structure(TRUE, m2_class = "m2_boolean")
        } else if (ret == "false") {
          ret <- m2_structure(FALSE, m2_class = "m2_boolean")
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
    m2_parse_sequence <- function(tokens, start = 1, save_paren = FALSE) {

      elem <- m2_parse_list(tokens, start = start, open_char = "(", close_char = ")", type_name = "sequence")

# if sequence has only one element
      if (length(elem$result) == 1 && !save_paren) {
        elem$result <- elem$result[[1]]
      }

      elem

    }



#' @rdname m2_parser
#' @export
    print.m2_integer <- function(x, ...) {
      if(inherits(x, "bigz")) return(get("print.bigz", envir = asNamespace("gmp"))(x))
        class(x) <- "numeric"
      print(x)
    }


#' @rdname m2_parser
#' @export
    print.m2_float <- function(x, ...) {
      class(x) <- "numeric"
      print(x)
    }


#' @rdname m2_parser
#' @export
    print.m2_complex <- function(x, ...) {
      class(x) <- "complex"
      print(x)
    }


#' @rdname m2_parser
#' @export
    print.m2_string <- function(x, ...) {
      class(x) <- "character"
      print(x)
    }

#' @rdname m2_parser
#' @export
    print.m2_boolean <- function(x, ...) {
      print(unclass(x))
    }


#' @rdname m2_parser
#' @export
    print.m2_list <- function(x, ...) {
      cat("M2 List\n")
      print(unclass(x))
    }


#' @rdname m2_parser
#' @export
    print.m2_array <- function(x, ...) {
      cat("M2 Array\n")
      print(unclass(x))
    }


#' @rdname m2_parser
#' @export
    print.m2_sequence <- function(x, ...) {
      cat("M2 Sequence\n")
      print(unclass(x))
    }


#' @rdname m2_parser
#' @export
    print.m2_symbol <- function(x, ...) {
      cat("M2 Symbol:", x, "\n")
      invisible(x)
    }


#' @rdname m2_parser
#' @export
    print.m2_option <- function(x, ...) {
      cat("M2 Option\n")
      cat(x[[1]], "=>", x[[2]])
    }


#' @rdname m2_parser
#' @export
    print.m2_hashtable <- function(x, ...) {
      cat("M2 HashTable\n")
      print(unclass(x))
    }

#' @rdname m2_parser
#' @export
    print.m2_module <- function(x, ...) {
      cat("M2 Module\n")
      print(unclass(x))
    }




    error_on_fail <- function(t, e) {
      if (!t) stop(e)
    }







#' @rdname m2_parser
#' @export
    m2_toggle_gmp <- function() {
      options <- getOption("m2r")
      options$gmp <- !options$gmp
      if (options$gmp) {
        message("m2r is now using gmp.")
      } else {
        message("m2r is no longer using gmp.")
      }
      options(m2r = options)
    }



#' @rdname m2_parser
#' @export
    get_m2_gmp <- function() getOption("m2r")$gmp






*/
