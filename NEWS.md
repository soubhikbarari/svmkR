# svmkR 1.1

* Added bug fix by [@felippelazar](https://github.com/soubhikbarari/svmkR/pull/27) to fix processing of drop-down menu type questions.

# svmkR v1.0

* **NEW:** Weight surveys using available population targets. Ex: 
  - `weight_to()`: simple canned function crosswalk a survey to an available target population and then weight to that distribution
  - `list_targets()`: list target population distributions available in our package
  - `read_target()`: read in a target available in our package

* **NEW:** Added ability to reverse engineer skip rules (`find_skip_rules()`) from survey responses based on completion patterns.

* **NEW:** Added demographic variable re-coding module

* Unit tests added for all modules.

* Stabilized some banner creation code.

* `pkgdown` web page created for package.

* End-to-end tutorial created via `vignette("svmkR")`.

# svmkR v0.3

* Added modules to create surveys from this package. Ex:
  - `read_qdoc()`: read a questionnaire from txt file
  - `upload_qdoc()`: upload questionnaire file
* Separate `write_banners()` into `write_banners()` and `upload_banners()`

# svmkR v0.2.2

* Fixed a bug in downloading a survey with named columns (i.e. `col_names = "name"`)
* `parse_survey()` properly parses multi-answer matrix questions (one column for every question-row-column triplet)

