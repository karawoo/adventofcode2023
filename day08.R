library("readr")
library("stringr")

dat <- read_lines("input08.txt")

Map <- R6::R6Class("Map", list(
  instructions = c(),
  map = list(),
  position = "AAA",
  step_count = 0,
  inst_count = 1,

  read_input = function() {
    dat <- read_lines("input08.txt")
    self$instructions <- str_split(dat[[1]], "")[[1]]
    map <- dat[3:length(dat)]
    map_names <- unlist(str_extract_all(map, "^[A-Z]{3}"))
    self$map <- map %>%
      str_remove_all("^[A-Z]{3} = \\(|\\)") %>%
      str_split("(, )") %>%
      setNames(map_names) %>%
      lapply(setNames, c("L", "R"))
    invisible(self)
  },

  new_position = function(instruction) {
    self$position <- self$map[[self$position]][[instruction]]
    self$step_count <- self$step_count + 1
    self$inst_count <- ifelse(
      self$inst_count == length(self$instructions),
      1,
      self$inst_count + 1
    )
    invisible(self)
  },

  run = function() {
    self$read_input()
    while (self$position != "ZZZ") {
      self$new_position(self$instructions[self$inst_count])
    }
    self$step_count
  }
))

m <- Map$new()
m$run()
