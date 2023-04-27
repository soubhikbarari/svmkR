# News

## svmkR v0.3

* Added modules to create surveys from this package. Ex:
  - `read_qdoc`: read a questionnaire from txt file
  - `upload_qdoc`: upload questionnaire file
* Separate `write_banners` into `write_banners` and `upload_banners`

## svmkR v0.2.2 (Release date: 2023-04-25)

* Fixed a bug in downloading a survey with named columns (i.e. `col_names = "name"`)
* `parse_survey()` properly parses multi-answer matrix questions (one column for every question-row-column triplet)