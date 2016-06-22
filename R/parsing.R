

# splits a string containing M2 code into tokens to ease parsing
# places an empty string at the start of each new line (including the first line)
m2_tokenize <- function(s) {
  varnamechars <- unlist(
    c(letters, toupper(letters), seq(10)-1, c("'"))
  )
  operators <- c(
    "===>", "<==>", "<===",
    "==>", "===", "=!=", "<==", "^**", "(*)", "..<",
    "||", "|-", ">>", ">=", "=>", "==", "<=", "<<", "<-", "++", "^^",
    "^*", "#?", "//", "**", "@@", "..", ".?", "!=", ":=", "->", "_*",
    "~", "|", ">", "=", "<", "+", "^", "%", "#", "&", "\\", "/", "*",
    "@", ".", "?", "!", ":", ";", ",", "-", "_")
  # operatorchars <- unlist(strsplit("=<>!&|_^{}[]()+-*/\\:;.,?`~@#$", "", fixed = TRUE))
  operatorstarts <- unlist(lapply(operators, function(s) substr(s,1,1)))

  tokens <- c("")

  i <- 1
  while (i <= nchar(s)) {
    curchar <- substr(s, i, i)
    if (curchar %in% varnamechars) {
      start <- i
      i <- i + 1
      while (i <= nchar(s) && substr(s, i, i) %in% varnamechars) {
        i <- i + 1
      }
      i <- i - 1
      end <- i

      tokens <- append(tokens, substr(s,start,end))
    }
    else if (curchar %in% operatorstarts) {
      # substr() is smart enough to not index past the end of the string
      for (op in operators) {
        if (op == substr(s, i, i + nchar(op) - 1)) {
          tokens <- append(tokens, op)
          i <- i + nchar(op) - 1
          break()
        }
      }
    }
    else if (curchar == "\"") {
      i <- i + 1

      start <- i
      while (i <= nchar(s) && substr(s, i, i) != "\"") {
        if (substr(s, i, i) == "\\") i <- i + 1
        i <- i + 1
      }
      end <- i - 1

      tokens <- append(tokens, c("\"", substr(s,start,end), "\""))
    }
    else if (curchar == "\n") {
      tokens <- append(tokens, "")
    }

    # skip other whitespace, etc.
    i <- i + 1
  }

  tokens
}


m2_parse <- function(tokens) {

}
