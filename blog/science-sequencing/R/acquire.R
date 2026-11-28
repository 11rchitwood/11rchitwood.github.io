# Run from blog/science-sequencing. An existing CASE JSON can be passed as argument 1.
library(readr)

framework_id <- "27a08dc6-416e-11e7-ba71-02bd89fdd987"
api <- "https://case.georgiastandards.org/ims/case/v1p1"
package_url <- paste(api, "CFPackages", framework_id, sep = "/")
courses <- c(Biology = "26.01200", Chemistry = "40.05100", Physics = "40.08100")
prefixes <- c(Biology = "SB", Chemistry = "SC", Physics = "SP")

args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  input <- args[[1]]
} else {
  input <- tempfile(fileext = ".json")
  download.file(package_url, input, mode = "wb", quiet = TRUE)
}

case <- jsonlite::fromJSON(input)
document <- case$CFDocument
stopifnot(identical(document$identifier, framework_id), document$adoptionStatus == "Adopted")
items <- case$CFItems
links <- subset(case$CFAssociations, associationType == "isChildOf")
children <- function(parent_ids) {
  ids <- links$originNodeURI$identifier[links$destinationNodeURI$identifier %in% parent_ids]
  items[match(ids, items$identifier), , drop = FALSE]
}

extract_course <- function(discipline) {
  course <- items[which(items$humanCodingScheme == courses[[discipline]]), ]
  stopifnot(nrow(course) == 1L)
  prefix <- prefixes[[discipline]]
  standard_pattern <- paste0("^", prefix, "[0-9]+$")
  standards <- children(course$identifier)
  standards <- standards[which(grepl(standard_pattern, standards$humanCodingScheme)), ]
  elements <- children(standards$identifier)
  stopifnot(
    nrow(standards) > 0L, nrow(elements) > 0L,
    all(grepl(paste0("^", prefix, "[0-9]+\\.[a-z]$"), elements$humanCodingScheme)),
    all(elements$CFItemType == "Element")
  )
  parent <- standards[match(sub("\\.[a-z]$", "", elements$humanCodingScheme),
                            standards$humanCodingScheme), ]
  stopifnot(all(!is.na(parent$identifier)), setequal(parent$identifier, standards$identifier))
  data.frame(
    discipline,
    standard_id = parent$humanCodingScheme,
    substandard_id = elements$humanCodingScheme,
    standard_text = elements$fullStatement,
    parent_standard_text = parent$fullStatement,
    source_url = elements$uri,
    source_version = paste0("Science GSE (2017-18); CASE ", document$lastChangeDateTime),
    item_last_modified = elements$lastChangeDateTime,
    course_code = course$humanCodingScheme,
    text_status = "official",
    stringsAsFactors = FALSE
  )
}

standards <- do.call(rbind, lapply(names(courses), extract_course))
standards <- standards[order(standards$discipline, standards$substandard_id), ]
stopifnot(!anyDuplicated(standards$substandard_id), all(nzchar(standards$standard_text)))
write_csv(standards, "data/standards.csv")
print(table(standards$discipline))
message("Source: ", package_url, " | Framework timestamp: ", document$lastChangeDateTime)
