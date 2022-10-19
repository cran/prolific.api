
## ---- package-load ----

library(prolific.api)
library(data.table)

## ---- start-date ----
start_date <- Sys.time()

## ---- access-create-anon ----
prolific_api_access <- api_access(api_token = "<api_token>")

## ---- access-create ----

suppressMessages(invisible(prolific_api_access <- api_access(api_token = readLines("~/.prolific_api/R_package_prolific.api_token", warn = FALSE))))

## ---- access_check_authorization ----

prolific_api_access$check_authorization()

## ---- access_get ----

prolific_api_access$access(
  method = "get",
  endpoint = "users/me"
)

## ---- access_post ----

prolific_api_access$access(
  endpoint = "study-cost-calculator",
  method = "post",
  data = list(
    reward = 100,
    total_available_places = 5
  )
)

## ---- study-create-simple-pre ----

pre_study <- prolific_study(
  # Information shown to participants
  name = "<Some older study>",
  description = "<Study description>",
  estimated_completion_time = 1,
  reward = 10,
  # URL participants are redirected to
  external_study_url = "https://www.link_to_my_older_study.com",
  # Completion code to verify participation
  completion_code = "ABC",
  # Number of participants to recruit
  total_available_places = 5
)
## ---- participant-id-prescreener ----

participant_id <-
  readLines("/Users/simonlenau/.prolific_api/participant_test_id")

participant_id_prescreener <-
  do.call(
    "prolific_prescreener",
    c(
      list(title = "Custom Whitelist"),
      as.list(participant_id)
    )
  )

## ---- study-create-simple ----

new_study <- prolific_study(
  # Information shown to participants
  name = "<Publicly visible study name>",
  description = "<Publicly visible study description>",
  estimated_completion_time = 1,
  reward = 10,
  # URL participants are redirected to
  external_study_url = "https://www.link_to_my_study.com",
  # Completion code to verify participation
  completion_code = "123",
  # Number of participants to recruit
  total_available_places = 10,
  eligibility_requirements =
    list(
      participant_id_prescreener
    )
)

## ---- study-post-simple-pre ----

prolific_api_access$access(
  endpoint = "studies",
  method = "post",
  data = pre_study
)

## ---- study-post-simple ----

prolific_api_access$access(
  endpoint = "studies",
  method = "post",
  data = new_study
)

## ---- study-list ----

# If the study ID is unknown, you can obtain a list of all studies:
list_of_studies <-
  prolific_api_access$access(
    endpoint = "studies",
    method = "get"
  )

## ---- study-list-modify ----

list_of_studies <- list_of_studies[id %in% c(new_study$id)]


## ---- study-list-print ----

print(list_of_studies)


## ---- study-get-function ----

study_get_function <- function(study) {
  return(
    c(
      "## ---- study-get ----",
      paste0("# Obtain the study with ID ", study$id, " from Prolific"),
      "obtained_study <- ",
      "\tprolific_api_access$access(",
      paste0("\t\tendpoint = c(\"studies\",\"", study$id, "\"),"),
      "\t\tmethod = \"get\"",
      ")"
    )
  )
}

## ---- study-rename ----

# S4 class style
name(new_study) <- "How to create and update studies on Prolific"
# Refclass style
new_study$name <- "How to create and update studies on Prolific"


## ---- study-patch-function ----

study_patch_function <- function(study, patch_study) {
  return(
    c(
      "## ---- study-patch ----",
      paste0("# Patch ", deparse(substitute(study)), " on Prolific"),
      "prolific_api_access$access(",
      paste0("\tendpoint = c(\"studies\",", deparse(substitute(study)), "$id),"),
      "\tmethod = \"patch\",",
      paste0(
        "\tdata = ", deparse(substitute(patch_study))
      ),
      ")"
    )
  )
}

## ---- study-delete-function ----

study_delete_function <- function(study) {
  return(
    c(
      "## ---- study-delete ----",
      paste0("# Delete ", deparse(substitute(study)), " on Prolific"),
      "prolific_api_access$access(",
      paste0("\tendpoint = c(\"studies\",", deparse(substitute(study)), "$id),"),
      "\tmethod = \"delete\"",
      ")"
    )
  )
}

## ---- prescreener-us ----

us_prescreener <-
  prolific_prescreener(
    title = "Current Country of Residence",
    "United States"
  )

## ---- prescreener-uk-us ----

uk_us_prescreener <-
  prolific_prescreener(
    title = "Current Country of Residence",
    "United Kingdom",
    "United States"
  )

## ---- prescreener-age ----

age_prescreener <-
  prolific_prescreener(
    title = "Age",
    "Minimum Age" = 20,
    "Maximum Age" = 24
  )

## ---- study-create-prescreener ----

new_study_with_prescreeners <- prolific_study(
  # Information shown to participants
  name = "<Publicly visible study name>",
  description = "<Publicly visible study description>",
  estimated_completion_time = 1,
  reward = 10,
  # URL participants are redirected to
  external_study_url = "https://www.link_to_my_study.com",
  # Completion code to verify participation
  completion_code = "123",
  # Number of participants to recruit
  total_available_places = 10,
  # Constraints that participants have to meet
  eligibility_requirements = list(
    uk_us_prescreener,
    age_prescreener
  )
)

## ---- study-post-prescreener ----

prolific_api_access$access(
  endpoint = "studies",
  method = "post",
  data = new_study_with_prescreeners
)

## ---- study-modify-prescreener-age ----

new_study_with_prescreeners$eligibility_requirements$
  `Age`$constraints <- list(
  "Minimum Age" = 18,
  "Maximum Age" = 28
)


## ---- study-modify-prescreener-us ----

new_study_with_prescreeners$eligibility_requirements$
  `Current Country of Residence` <- us_prescreener


## ---- delete-all-studies ----

list_of_studies <-
  prolific_api_access$access(
    endpoint = "studies",
    method = "get",
    as_list = TRUE
  )

lapply(
  list_of_studies[creation_day >= as.IDate(start_date) &
    creation_time >= as.ITime(start_date) - as.ITime(as.POSIXlt("3", format = "%H")), ]$id,
  function(id) {
    prolific_api_access$access(
      endpoint = c("studies", id),
      method = "delete"
    )
  }
)


## ---- api_token_s4 ----
api_token(prolific_api_access)
## ---- api_token_rc ----
prolific_api_access$api_token
## ---- api_token_s4_assign ----
api_token(prolific_api_access) <- "<new_api_token>"
## ---- api_token_rc_assign ----
prolific_api_access$api_token <- "<new_api_token>"
