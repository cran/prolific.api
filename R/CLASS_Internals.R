#' Internal class
#'
#' @description
#' Class that is used to store private fields and methods
#' To be used inside other classes only
#'
#' @field fields
#' A list that contains the private fields, as well as their respective restrictions
#' (e.g. type, size, ...)
#'
#' @field methods
#' A list that contains the private methods
#'
#' @noRd
..internals <- setRefClass(
    Class = "..internals",
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Fields Block (setter/getter functions)
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    fields = list(
        fields = function(value) .accessField("fields", value, .self, TRUE),
        methods = function(value) .accessField("methods", value, .self, TRUE)
    ),
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    # Methods Block
    # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
    methods = list(
        # Initializer
        initialize =
            function(fields = NULL,
                     methods = NULL) {
                # Define Attribute restrictions
                assign(
                    ".field_restrictions",
                    list(
                        fields = list(class = "environment"),
                        methods = list(class = "environment")
                    ),
                    .self
                )

                # Assignments
                fields <<- new.env(.self)
                methods <<- new.env(.self)
            }
    )
)
