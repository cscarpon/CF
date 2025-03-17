mo <- setRefClass(
  "MetaObject",
  fields = list(
    filepath = "character",
    metadata = "data.frame"
  ),
  methods = list(
    initialize = function(file_path = character(0)) {
      if (length(file_path) > 0) {
        .self$filepath <- file_path
        .self$metadata <- extract_info(file_path)
      }
    }
  )
)
