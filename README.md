The CHat Assisted Debugging (chad) package is an attempt to make debugging in R more user-friendly. It works by submitting error and stack trace messages to OpenAI to provide human-readable error interpretation, context, and debugging advice. It depends on the `cli`, `rlang`, `jsonlite` and `OpenAI` packages as well as an active [OpenAI API key](https://platform.openai.com/api-keys).

To install the package run:
```
library(devtools)
install_github("n8layman/chad")
```

To use:
1. Add your OpenAI API key to the global environment using `Sys.setenv(OPENAI_API_KEY = "sk-xxxx")` where "sk-xxx" is your OpenAI API key.
2. Run rlang::global_entrace() _before_ running the code that you suspect will generate an error.
3. After encountering an error run `explain_last_error()`. The ChatGPT enhanced error will be displayed as a message and returned invisibly for manual printing.
