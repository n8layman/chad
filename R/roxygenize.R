#' Automatically Generate Roxygen Headers For a Given Function
#'
#' This function parses a given function definition and automatically constructs roxygen headers
#' using any provided information such as required_tags and author details.
#' It can handle varying cases where tags are required but not provided.
#'
#' @author Nathan C. Layman
#'
#' @param func Function definition that needs to be parsed for roxygen header construction.
#' @param required_tags Character vector. Default values are "author", "param", "return", "note", "examples",
#' and "export". Tags entered here should be in order.
#' @param provided_tags List. Author tag details to be parsed onto the roxygen header of the function. Default is "Nathan C. Layman".
#' @param model Character. OpenAI model to be used. Default is 'gpt-4'.
#' @param prompt Character. The prompt defines the context for the openai model to generate the Roxygen documentation.
#'
#' @return Returns roxygen headers that were constructed for the provided function.
#'
#' @note User should ensure that the function definition, tag details and Openai API key are properly formatted and available
#' for this function to run smoothly.
#'
#' @examples
#' roxygenize(`function (func, required_tags = c("author", "param", "return",
#' "note", "examples", "export"), provided_tags = list(author = "Nathan C. Layman"),
#'model = "gpt-4", prompt = "You parse a function definition to automatically construct roxygen headers returning only the header.")
#'`,
#' required_tags  = c("author", "param", "return", "note", "examples", "export"),
#' provided_tags = list(author = "Nathan C. Layman"),
#' model = "gpt-4",
#' prompt = "You parse a function definition to automatically construct roxygen headers returning only the header.")
#'
#' @export
roxygenize <- function(func,
                       required_tags = c("author","param","return","note","examples","export"),
                       provided_tags = list(author = "Nathan C. Layman"), # Named list of any provided tags as key-value pairs
                       model = "gpt-4",
                       prompt = "You parse a function definition to automatically construct roxygen headers returning only the header.") {

  if(!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    warning(cli::cli_text("OpenAI API key not found! To remedy:\n
            \t 1. Visit {.url https://platform.openai.com/api-keys} to create an account and request an API key\n
            \t 2. Run `Sys.setenv(OPENAI_API_KEY = 'sk-xxxx')` where 'sk-xxx' is the API key provided by OpenAI or add your key to your .env file or .RProfile \n
            \t 3. Try `explain_last_error()` again\n"))
    return()
  }

  # Figure out name of function
  func_name <- deparse(substitute(func))

  # Deparse the function code into a character vector
  func_code <- deparse(func)

  # Collapse the vector into a single string with newlines
  func_code_string <- paste("Here is the function definition:", func_code, sep = "\n", collapse = "\n")

  # Figure out function arguments
  func_args <- formals(func)
  params <- paste(names(func_args), collapse = ", ")

  required_tags <-  c("author",
                      "param",
                      "return",
                      "note",
                      "examples",
                      "export")

  # Dynamic context message for the model prompt
  context <- glue::glue(
    "Generate a complete Roxygen documentation header for the provided R function.
    The function is named '{func_name}' and has the following parameters: {params}.
    Come up with a title and description and include them at the top of the header every time.
    A title and description must be included but should not be preceded by an '@' tag.
    The required tags include: {paste(required_tags, collapse = ', ')} in that order.
    Please use the function definition provided to fill in the details for these tags.
    Separate diffent discrete tags with an empty roxygen header line. For example @author
    and @param should be separated but not different @param tags.
    Be concise but complete in your descriptions of the parameters, return values, and examples.
    Return only the required header and nothing else."
  )

  example <- "#' Retrieve and preprocess global elevation data\n#'\n#' This function downloads global elevation data, transforms it, and saves it as an optimized Parquet file\n#' and a TIF file in the specified directory. If a file already exists at the target filepath and overwrite\n#' is FALSE, the existing file is returned.\n#'\n#' @author Nathan C. Layman\n#'\n#' @param output_dir Directory where the processed files will be saved. This directory is created if it doesn't exist.\n#' @param output_filename Desired filename for the processed file.\n#' @param continent_raster_template Template to be used for terra raster operations.\n#' @param overwrite Boolean flag indicating whether existing processed files should be overwritten. Default is FALSE.\n#' @param ... Additional arguments not used by this function but included for generic function compatibility.\n#'\n#' @return A string containing the filepath to the processed file.\n#'\n#' @note This function creates a new directory, downloads elevation data, processes and saves results\n#' as parquet and tif files in the specified directory. If a file already exists at the target filepath and\n#' overwrite is FALSE, the existing file is returned.\n#'\n#' @examples\n#' get_elevation_data(output_dir = './data',\n#'                    output_filename = 'elevation.parquet',\n#' continent_raster_template = raster_template,\n#' overwrite = TRUE)\n#'\n#' @export"

  messages <-
    list(
      list(
        "role" = "system",
        "content" = prompt
      ),
      list(
        "role" = "user",
        "content" = context
      ),
      list(
        "role" = "user",
        "content" = example
      )
    )

  if(!is.null(provided_tags)) {
    provided_tags_message <- paste(paste0("@",names(provided_tags)), provided_tags, sep = " ", collapse = ", ")
    provided_tags_message <- glue::glue("Provided tags: {provided_tags_message}")
    messages <- c(messages, list(list("role" = "user", "content" = provided_tags_message)))
  }

  messages <- c(messages, list(list(
    "role" = "user",
    "content" = func_code_string
  )))

  roxygen_header <- openai::create_chat_completion(
    model = model,
    messages = messages)['choices'][[1]]$message.content

  cat(roxygen_header)
}

