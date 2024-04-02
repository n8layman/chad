The CHat Assisted Debugging (chad) package is an attempt to make debugging in R more user-friendly. It works by submitting error and stack trace messages to OpenAI to provide human-readable error interpretation, context, and debugging advice. It depends on the `cli`, `rlang`, and `OpenAI` packages as well as an active [OpenAI API key](https://platform.openai.com/api-keys).

This package requires that the `rlang::global_entrace()` function is run _before_ running code you would like to error check to enable rlang-enhanced error messaging. 
