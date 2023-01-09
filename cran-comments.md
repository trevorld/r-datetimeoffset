Responses to Victoria Wimmer's submission comments:

* Thanks!
* We've tweaked the DESCRIPTION Description so it doesn't start with a 
  repeat of the package title.
* We've added a value Rd tag to `weekdays.datetimeoffset.Rd`
* We've tweaked the comment at the beginning of the `\dontrun{}` block in `datetime_cast.Rd`.
  If not included in a `\dontrun{}` block this block will throw a R CMD check ERROR 
  since the wrapped R code throws an error.
  This block documents one of the key behavioural differences between 
  `{clock}`'s datetime casting (which throws an error in this case) 
  and our package---`{datetimeoffset}`'s---datetime casting (which doesn't throw an error).

## Test environments

* local (linux, R 4.2.2)
* win-builder (windows, R devel)
* github actions (windows, R release)
* github actions (macOS, R release)
* github actions (linux, R devel)
* github actions (linux, R release)
* github actions (linux, R oldrel)

## R CMD check --as-cran results

* NOTE: This is a new submission
* NOTE: Possibly misspelled words "datetimes" and "datetime" (but they are not misspelled)
