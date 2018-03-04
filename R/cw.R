#' Console wrap. Formats a string for printing to the console window
#'
#' Takes a potentially long character object and adds carriage returns and blank spaces for better
#' appearance when printing to the terminal window
#'
#' @param x A character object
#' @param final.cr Whether to add a carriage return at the end. T/F.
#' @param indent First line indent (number of blank spaces)
#' @param exdent Subsequent line(s) indent (number of blank spaces)
#'
#' @note
#' This function can prevent long messages from extending beyond the width of the R-console.
#' Works with any function that prints to the console (e.g., cat, print, stop)
#' 
#' @return character object with embedded \\n characters
#'
#' @export

cw <- function(x, final.cr = TRUE, indent=0, exdent=0) {
  
  ## Console wrap. Wraps a long string for the console width. Returns a character string with \n characters at the end of each line
  ## Works well for any command that prints to the console (cat, print, stop)
  ## final.cr - if TRUE adds a carriage return character at the end (use in conjunction with 'cat()'
  ## When using as the argument for stop() function, set exdent=2
  
  return(paste(paste(strwrap(x, indent=indent, exdent=exdent), sep="", collapse="\n"), ifelse(final.cr,"\n",""), sep=""))
  
}


