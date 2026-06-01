suppressPackageStartupMessages({
  suppressMessages({
    suppressWarnings({
      library(RefManageR)
      library(whisker)
      library(stringr)
      library(glue)
      library(purrr)
    })
  })
})

#' Format author name with optional bold formatting
#'
#' This function takes an author object, formats the name, and applies bold formatting if the name contains "Rahul".
#'
#' @param author A list containing 'given' and 'family' name components
#' @return A character string of the formatted name, potentially in bold
#' @importFrom glue glue glue_collapse
#' @importFrom stringr str_detect
#' @examples
#' author <- list(given = c("John", "A."), family = "Doe")
#' help_name(author)
#'
#' author <- list(given = "Rahul", family = "Smith")
#' help_name(author)
help_name <- function(author) {
  given <- glue_collapse(str_squish(author$given), sep = " ")
  family <- glue_collapse(str_squish(author$family), sep = " ")
  chr_name <- glue("{given} {family}") |>
    str_squish()
  message(chr_name)
  flag_rahul <- str_detect(
    tolower(chr_name), "rahul"
  )
  if (flag_rahul) {
    return(glue("**{chr_name}**"))
  } else {
    return(chr_name)
  }
}


#' Format a bibliographic entry in APA style
#'
#' This function takes a bibliographic entry and formats it according to APA style,
#' with different formatting for articles and other types of entries (e.g., presentations, posters).
#'
#' @param entry A list containing bibliographic information (e.g., author, year, title)
#' @param type A character string specifying the type of entry ("article" or other)
#'
#' @return A character string of the formatted bibliographic entry
#'
#' @importFrom purrr map_chr
#' @importFrom glue glue
#' @importFrom stringr str_c str_glue
#'
#' @examples
#' article_entry <- list(
#'   author = list(list(given = "John", family = "Doe"), list(given = "Jane", family = "Smith")),
#'   year = "2023",
#'   title = "Example article title",
#'   journal = "Journal of Examples",
#'   volume = "10",
#'   number = "2",
#'   pages = "100-110"
#' )
#' format_entry(article_entry, "article")
#'
#' presentation_entry <- list(
#'   author = list(list(given = "Alice", family = "Johnson")),
#'   year = "2023",
#'   month = "June",
#'   title = "Example presentation title",
#'   btype = "Paper presentation",
#'   note = "Annual Conference on Examples",
#'   location = "New York, NY",
#'   date = "June 15-17",
#'   url = "https://example.com/presentation"
#' )
#' format_entry(presentation_entry, "presentation")
#'
#' @export
format_entry <- function(entry, type) {
  bold_authors <- map_chr(entry$author, help_name)
  if (type == "article") {
    out <- paste(
      paste(bold_authors, collapse = ", "), # Author names
      " (", entry$year, "). ", # Year in parentheses
      entry$title, ". ", # Title in sentence case
      if (!is.null(entry$journal)) paste0("*", entry$journal, "*") else "", # Journal name in italics
      if (!is.null(entry$volume)) {
        paste0(", *", entry$volume, "*") # Volume in italics
      } else {
        ""
      },
      if (!is.null(entry$number)) {
        paste0("(", entry$number, ")") # Issue in parentheses, not italicized
      } else {
        ""
      },
      if (!is.null(entry$pages)) paste0(", ", entry$pages), # Page numbers
      ".",
      sep = ""
    )
    out <- str_replace_all(out, "\\&")
  } else {
    out <- glue::glue(
      "{str_c(bold_authors, collapse = ', & ')} ({entry$year}{if (!is.null(entry$month)) str_glue('{, entry$month}') else ')'}. ", # Author names, year, and month
      "{entry$title}. ", # Title in sentence case
      "{if (!is.null({entry$btype})) str_glue('[{entry$btype}]. ') else ''}", # Type of presentation (e.g., Poster presentation, Paper presentation)
      "{entry$note}", # Conference or event name
      "{if (!is.null(entry$location)) str_glue(', {entry$location}') else ''}", # Location of the event
      "{if (!is.null(entry$date)) str_glue('{entry$date}') else ''}", # Date range of the event
      "{if (!is.null(entry$url)) str_glue('{entry$url}') else ''}" # URL if available
    )
    out <- str_replace_all(out, "\\&")
  }
  return(out)
}

#' Get unique entries of a specific type, sorted by year
#'
#' This function filters bibliographic entries by type, removes duplicates,
#' sorts them by year in descending order, and formats each entry.
#'
#' @param bib A bibliography object (e.g., from RefManageR)
#' @param type The type of entries to filter (e.g., "article", "inproceedings")
#' @return A list of formatted bibliographic entries
#' @examples
#' # Assuming 'bib' is a bibliography object
#' get_entries(bib, "article")
get_entries <- function(bib, type) {
  entries <- bib[type = type]
  # Remove duplicates based on title and year
  unique_entries <- entries[!duplicated(sapply(entries, function(x) paste(x$title, x$year)))]
  # Sort entries by year in descending order
  sorted_entries <- unique_entries[order(sapply(unique_entries, function(x) as.numeric(x$year)), decreasing = TRUE)]
  lapply(sorted_entries, format_entry, type)
}


# Read the BibTeX file
bib <- ReadBib("paperbibcite.bib", check = FALSE)

# Prepare data for the template
data <- list(
  journal_publications = get_entries(bib, "article"),
  presentations = get_entries(bib, "presentations"),
  posters = get_entries(bib, "posters"),
  teaching = get_entries(bib, "teaching")
)

# Read the Whisker template
template <- readLines("templates/template.qmd")

# Render the template
output <- whisker.render(template, data)

# Write the output to publications.qmd
writeLines(output, "stage/publications.qmd")
