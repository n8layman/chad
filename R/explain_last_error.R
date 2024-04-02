#' Use ChatGPT to explain the last error
#'
#' @return
#' @export
#'
#' @examples
explain_last_error <- function() {

  if(!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    warning(cli::cli_text("OpenAI API key not found! To remedy:\n
            \t 1. Visit {.url https://platform.openai.com/api-keys} to create an account and request an API key\n
            \t 2. Run `Sys.setenv(OPENAI_API_KEY = 'sk-xxxx')` where 'sk-xxx' is the API key provided by OpenAI or add your key to your .env file or .RProfile \n
            \t 3. Try `explain_last_error()` again\n"))
    return()
  }

  e <- tryCatch({
    rlang::last_error()
  },
  error = function(e) {
    message("This function depends on rlang enriched errors. Please run `rlang::global_entrace()` or add it to your .RProfile before running whatever code generated the error you want to explain. Loading it now.`\n")
    rlang::global_entrace()
    e
  })

  messages <-
    list(
      list(
        "role" = "system",
        "content" = "You provide human readable debugging help and a list of troubleshooting steps for the following error and stack trace in the R programing language."
      ),
      list(
        "role" = "user",
        "content" = paste("error:", e)
      )
    )

  err <- openai::create_chat_completion(
    model = "gpt-3.5-turbo",
    messages = messages)['choices'][[1]]$message.content

  print(e$trace)
  err <- paste("\nError:", e$message, "\n\nExplanation:", err |> unlist())

  message(err)
  invisible(err)
}
