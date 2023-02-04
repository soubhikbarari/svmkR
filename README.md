
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `svmkR` :clipboard: :monkey:

<!-- badges: start -->

[![codecov.io](https://codecov.io/github/tntp/surveymonkey/coverage.svg?branch=master)](https://codecov.io/github/tntp/surveymonkey?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/tntp/surveymonkey/workflows/R-CMD-check/badge.svg)](https://github.com/tntp/surveymonkey)
<!-- badges: end -->

This package provides access from R to the SurveyMonkey API. You can browse your surveys, pick one to fetch, return to them to `data.frame` format. As of 1.1.3, `svmkR` includes additional utilities to (1) analyze surveys and (2) create nice banners.

*This is a fork of [Sam Firke / Matt Roumaya’s SurveyMonkey R package](https://github.com/mattroumaya/surveymonkey), optimized for internal usage by the SurveyMonkey Research Insights Team.*

### Authors

The [first version of this package](https://github.com/cloudyr/Rmonkey)
was written by Thomas Leeper. It worked with version 2 of the
SurveyMonkey API.

After SurveyMonkey’s change to v3 of their API broke the package, it was
rewritten by employees of [TNTP](https://tntp.org), a nonprofit company
working to end the injustice of educational inequality.

Matt Roumaya took over from TNTP as the de facto maintainer in 2021 and,
in 2022, became the official maintainer of the package, keeping it going
into another stage of its life.

*This version was forked from the TNTP version in July 2022 by Soubhik
Barari while at Momentive.ai (maker of SurveyMonkey) and optimized for
internal usage (e.g., faster reads, more verbose outputs, handling of
API errors and edge cases, etc.).*

## Installation

This package is not yet on CRAN. Install from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("tgravelle/svmkR")
```

## Usage

### Authentication

*Have an opinion about OAuth procedures? If you can improve this
guidance, please open an issue with your suggestions.*

#### Get your OAuth token

You’ll need an OAuth token, and for that you’ll need to set up an app.

Log in to SurveyMonkey in your browser, then navigate to
<https://developer.surveymonkey.com/apps>. Create an app. It should be
private, and you should enable the relevant scopes: View Surveys, View
Collectors, View Contacts, View Responses, View Response Details. (That
should do it, but if you get a 403 error when you try to browse surveys,
try enabling other relevant scopes). You don’t need to click “deploy”,
as long as the setting selected have been updated you’re set.

Now look at the settings page for your app and take note of the “Access
Token” field, which should contain a very long character string.

#### Add your OAuth token to your .Rprofile

Add the SurveyMonkey account’s OAuth token to your .Rprofile file. To
open and edit that file, run `usethis::edit_r_profile()`, then add a
line like this:
`options(sm_oauth_token = "kmda332fkdlakfld8am7ml3dafka-dafknalkfmalkfad-THIS IS NOT THE REAL KEY THOUGH")`.

Except use the OAuth token listed on your app’s settings page, obtained
in the previous step.

Restart R for this change to take effect.

If this is all set up successfully, the token will print when you run
`getOption("sm_oauth_token")`. Guard this token: don’t share it and
don’t commit it in any repository.

*Developer’s note: That’s how a user can get a single account’s OAuth
token. It might be preferable if users could authenticate from within R.
If someone has guidance for how users should obtain their OAuth token
more generally, please submit a pull request or comment in an issue.*

### Browsing your surveys

You’ll need the ID number of the survey you wish to fetch. Find it by
browsing your surveys like this:

``` r
surveys <- browse_surveys(200) # see your most recent 200 surveys
```

Then run `View(surveys)` and start typing the name of the survey you
want into the search window at the top right of the RStudio data.frame
viewer. That will get you the survey’s ID number. Copy it.

### Fetching a survey

Get the survey data like this:

``` r
a_survey_obj <- fetch_survey_obj(123456789) # your survey's ID goes here
```

This returns a list of data about the survey. It’s useful for developers
to explore, but not so much for most users. Keep going to the next step.

### Parsing the survey into a data.frame

This is the actual good part.

``` r
survey_df <- parse_survey(a_survey_obj)
```

That will give you a tidy data.frame with all of your responses.

In the future you can run it all as one command:

``` r
survey_df <- 123456789 %>%
  fetch_survey_obj %>%
  parse_survey
```

### Retrieving recipient and collector data

This is handy for tracking who has completed a survey and managing
reminder messages. Or retrieving a recipient’s unique survey link, if
you’re sending invitation through an email collector.

Get a survey’s collector information, including collector IDs:

``` r
collectors <- get_collectors(123456789)
```

Then fetch a collector’s recipient info:

``` r
recipients <- get_recipients(234567890) # use a collector ID retrieved in the previous step
```

### Removing HTML tags from column names

If your question text has bold font or other formatting, an HTML tag
will likely carry through. You can remove any text between “\<” and “\>”
with `strip_html()`.

``` r
survey_df <- 123456789 %>%
  fetch_survey_obj %>%
  parse_survey %>% 
  strip_html
```

### Estimating margin of error

There are two (cutely named, if we do say so ourselves) twin functions you can use to estimate [margin of error](https://en.wikipedia.org/wiki/Margin_of_error) (MOE) for your survey.

* Estimate the MOE using an asymptotic formula (`esti_moe`)
* Simulate the MOE using non-parametric bootstraps (`simu_moe`)

Here's an example using some dummy data included in the package:

```r
data(auto.evs)
esti_moe(auto.evs$weight_genpop)
simu_moe(auto.evs$weight_genpop)
```

### Making banners

Banners are the bread and butter of the SurveyMonkey Research Insights Team. 

To see how the `svmkR` package can do this, check out `make_banners()` and `write_banners()`.

## API considerations

Your account will likely be limited to 500 hits per day to the API. This
package will print reminders of how many calls you have left in the day.
The main thing to keep an eye on is respondent counts; as only 100
responses can be fetched per API call, a survey with X respondents will
make at least X/100 calls to the API.

# Contact

Something may not work, because this is an in-development package. Or
you may have an idea for something that hasn’t been implemented yet. In
either case, please create an issue in the GitHub issue tracker!
