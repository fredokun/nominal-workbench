type info = String
type error = Error_code.error_code * info

exception Error of error