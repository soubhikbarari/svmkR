# svmkR: Tools for SurveyMonkey Surveys in R :clipboard: :monkey:

<img src="hex.png" align="right" style="height: 128px; margin-left: 4px;"/>

This package provides a suite of tools to work with SurveyMonkey surveys.

You can:

* Create and upload a survey from a questionnaire document.
* Browse and download surveys in your account.
* Conduct basic analysis (e.g. margin of error) on your surveys
* Create presentable SurveyMonkey-style banners for polls.

**Note: This is the development branch, where new features for the coming release are being implemented and tested.**

## Installation

To install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("soubhikbarari/svmkR", branch = "dev")
```

## Usage

### Authentication

You’ll need an OAuth token, and for that you’ll need to set up an app.

1. Log in to SurveyMonkey in your browser, then navigate to
<https://developer.surveymonkey.com/apps>. 

2. Create an app. It should be private, and you should enable the relevant scopes: View Surveys, View
Collectors, View Contacts, View Responses, View Response Details. (That
should do it, but if you get a 403 error when you try to browse surveys,
try enabling other relevant scopes). You don’t need to click “deploy”,
as long as the setting selected have been updated you’re set.

3. Now look at the settings page for your app and take note of the “Access
Token” field, which should contain a very long character string.

4. Add the SurveyMonkey account’s OAuth token to your .Rprofile file. To
open and edit that file, run `usethis::edit_r_profile()`, then add a
line like this: `options(sm_oauth_token = "MY-OAUTH-TOKEN-FROM-STEP-3")`.

5. Restart R for this change to take effect.

6. If this is all set up successfully, the token will print when you run
`getOption("sm_oauth_token")`. Guard this token: don’t share it and
don’t commit it in any repository.

### Make a survey

<!-- TODO -->

### Upload your survey

<!-- TODO -->

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

### Estimating margin of error

There are two twin functions you can use to estimate [margin of error](https://en.wikipedia.org/wiki/Margin_of_error) (MOE) for your survey.

* Estimate the MOE using an asymptotic formula (`esti_moe`)
* Simulate the MOE using non-parametric bootstraps (`simu_moe`)

Both incorporate any weights estimated for the survey.

Here's an example using some dummy data included in the package:

```r
data(auto.evs)
esti_moe(auto.evs$weight_genpop)
simu_moe(auto.evs$weight_genpop)
```

### Making banners

Banners are the bread and butter of the SurveyMonkey Research Insights Team. 

To see how the `svmkR` package can do this, check out `make_banners()` and `write_banners()`.

## API Considerations

Your account will likely be limited to 500 hits per day to the API. This
package will print reminders of how many calls you have left in the day.

The main thing to keep an eye on is respondent counts; as only 100
responses can be fetched per API call, a survey with X respondents will
make at least X/100 calls to the API.

## Authors

**`svmkR` was forked from the `surveymonkey` package in July 2022 [Soubhik Barari](https://github.com/soubhikbarari) while at Momentive.ai (maker of SurveyMonkey) and significantly re-designed and optimized (e.g. faster reads, more verbose outputs, handling of API errors and edge cases, etc.) with contributions from [Christopher Remmel](https://github.com/calremmel).**

Previous versions of this package were written/maintained by [Thomas Leeper](https://github.com/leeper), [Sam Firke](https://github.com/sfirke), and [Matt Roumaya](https://github.com/mattroumaya).