#' Parse a string to extract value, lower and upper bounds
#
#' @param x A character string to parse.
#' @return A list with value, lower and upper bounds.
parse_value_string <- function(x) {
  
  print(cat('Parsing: ', x, '\n'))
  # Remove percentage signs
  x <- gsub("%", "", as.character(x))
  
  # Extract numbers from the string
  nums <- as.numeric(unlist(regmatches(x, gregexpr("[-+]?[0-9]*\\.?[0-9]+", x))))
  
  if (length(nums) == 3) {
    return(list(value = nums[1], lower = nums[2], upper = nums[3]))
  } else if (length(nums) == 4) {
    # Handle cases like "RR 1.5 (1.1,2.0)"
    return(list(value = nums[2], lower = nums[3], upper = nums[4]))
  } else {
    return(list(value = NA, lower = NA, upper = NA))
  }
}
