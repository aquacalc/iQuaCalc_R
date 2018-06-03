# Idea for better unit conversions
# iQuaCalc Unit Conversion Scratchpad
# menu of types of conversions, text-completion, echo entry
# et...voi-effing-là !

# library(stringr)
# library(dplyr)

# m.constant = Float.parseFloat(match(   "([-+]?[\\d\\.]+)[A-Za-z]?"   , lastMember, 1));
# m.variable = match(                    "[\\d\\.]+([A-Za-z]?)"        , lastMember, 1);
# or... "([^\\d\\.\\+\\-])+"
# my_eqn <- "4 * x + 72   * y"

# my_coefficients <- str_extract_all(my_eqn, "([-+]?[\\d\\.]+)[A-Za-z]?")[[1]]
# my_variables    <- str_extract_all(my_eqn, '([^\\d\\.\\+\\-])+')[[1]]


# scratchpad function to convert ALL UNITS ----

get_coefficient_and_units_all <- function(my_str, type) {
  
  # my_value <- str_extract_all(my_qty_to_convert, "([-+]?[\\d\\.]+)[A-Za-z]?")[[1]]
  my_value <- str_extract_all(my_str, "([-+]?[\\d\\.]+)")[[1]]   # allows '5.73 ft' and '5.73ft
  my_units <- str_extract_all(my_str, '([^\\d\\.\\+\\-])+')[[1]]
  
  coef  <- as.numeric(my_value)
  units <- trimws(my_units)
  
  # see: https://stackoverflow.com/questions/9057006/getting-strings-recognized-as-variable-names-in-r
  type_data <- eval(as.name(paste0(type, '.data')))
  
  cat('Convert ', coef, ' ', units, '\n')
  
  all_df <- convertAll(coef, units, type_data)
  
  return(all_df)
}

# CASE: User selects LENGTH radio button and types in '5.73 meters'...
rb_conversion <- 'area'

my_qty_to_convert <- '5.73 ft²'
# my_qty_to_convert <- '5.73 m²'

term_to_convert <- get_coefficient_and_units_all(my_qty_to_convert, rb_conversion)

print(term_to_convert)

# --------------------------------------------------------------

# "5.73 ft² to m²" or "5.73 ft², m²" or "5.73 ft² m²"

# test non-numeric part...
test_input <- '5.73 ft² to  m²   '
test_input

# extract non-numeric part(s), i.e., the units
my_units <- str_extract_all(test_input, '([^\\d\\.\\+\\-])+')[[1]]
my_units

# split on space(s)
all_units <- str_split(trimws(my_units, 'both'), ' ')[[1]]
all_units

# number of elements -- want 1st (FROM units) & last (TO units)
num_unit_elements <- length(all_units)
num_unit_elements

from_units <- all_units[1]
to_units   <- all_units[num_unit_elements]

cat('from ', from_units, ' to ', to_units, '\n')


# scratchpad function to convert to ONE UNIT ----

get_coefficient_and_units <- function(my_str, type) {
  
  # my_value <- str_extract_all(my_qty_to_convert, "([-+]?[\\d\\.]+)[A-Za-z]?")[[1]]
  my_value <- str_extract_all(my_str, "([-+]?[\\d\\.]+)")[[1]]   # allows '5.73 ft' and '5.73ft
  my_units <- str_extract_all(my_str, '([^\\d\\.\\+\\-])+')[[1]]
  
  coef  <- as.numeric(my_value)
  
  # split on space(s)
  all_units <- str_split(trimws(my_units, 'both'), ' ')[[1]]
  # all_units
  
  # number of elements -- want 1st (FROM units) & last (TO units)
  num_unit_elements <- length(all_units)
  # num_unit_elements
  
  from_units <- all_units[1]
  to_units   <- all_units[num_unit_elements]
  
  # see: https://stackoverflow.com/questions/9057006/getting-strings-recognized-as-variable-names-in-r
  type_data <- eval(as.name(paste0(type, '.data')))
  
  
  ans <- convert(coef, from_units, to_units, type_data)
  
  
  # cat('Convert: ', coef, ' ', from_units, ' -> ', ans, ' ', to_units, '\n')
  
  return(ans)
}


# CASE: User selects LENGTH radio button and types in '5.73 meters'...
rb_conversion <- 'area'

test_input <- '5.73 ft² to  m²   '
test_input <- '7.5 ha  acre'         # ans: 18.532904 acre
# test_input

my_conversion <- get_coefficient_and_units(test_input, rb_conversion)

my_conversion

# --------------------------------------------------------------
