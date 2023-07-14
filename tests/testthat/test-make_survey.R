test_that("test single choice questions", {
  
  q.text <- "
1. This is a multiple choice question. Every question starts with a
number followed by a period. There should be a blank line between
the question text and the choices.

a
b
c
d"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
  
  q.text <- "
[[Question:MC:Dropdown]]
drop down
[[Choices]]
a
b
c"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
  
  q.text <- "
[[Question:MC]]
This is a multiple choice question. With one value recoded.

[[AdvancedChoices]]
[[Choice]]
a choice
with text on
multiple lines
[[Choice]]
b
[[Choice]]
c
[[Choice:99]]
N/A with recode 99s
"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
})

test_that("test multiple choice questions", {
  
  q.text <- "
[[Question:MC:MultipleAnswer:Horizontal]]
multiple answer horizontal
[[Choices]]
a
b
c"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
  
  q.text <- "
[[Question:MC:MultiSelect]]
multiselect
[[Choices]]
a
b
c"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
})

test_that("test matrix questions", {
  
  q.text <- "
3. This is a matrix question that has longer question text.
It is a matrix question because it has two groups of choices.
The choices ma, mb, and mc are statements while m1, m2, and m3
are scale points. 


ma
mb
mc

m1234r23r2r3r32
m2
m3
ergerg
erger
"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
  
  q.text <- "
[[Question:Matrix]]
This question is a matrix question.

It has lots of question text on multiple lines and uses
advanced answers.

[[Choices]]
statement a
statement b
statement c
[[AdvancedAnswers]]
[[Answer]]
answer 1
[[Answer]]
answer 2
[[Answer]]
answer 3
"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
  
  q.text <- "
[[Question:Matrix]]
This question is a matrix question and uses advanced choices and 
answers with recode values.

[[AdvancedChoices]]
[[Choice]]
a
[[Choice]]
b
[[Choice]]
c
[[Choice]]
d

[[AdvancedAnswers]]
[[Answer:10]]
answer 1 - recode 10
[[Answer:20]]
answer 2 - recode 20
[[Answer:30]]
answer 3 - recode 30
"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
})

test_that("test open-ended", {
  q.text <- "
[[Question:Open]]
How do you feel about this survey?

"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
  
  q.text <- "
How do you feel about this survey? [[TE]]

"
  expect_warning((out <- as.qdoc.question(q.text)), NA)
})

test_that("test question bank", {
  httptest::with_mock_api({
    (out <- suppressWarnings(browse_question_bank("test token")))
  })
  expect_true("data.frame" %in% class(out))
  expect_gt(nrow(out), 0)
})


test_that("test question bank ID substitution", {
  q.text <- "
4. This is a matrix-multiple answer question. 
Some of the row text is drawn from the question bank via question bank ID tags.
[[MultipleAnswer]]

[[ID:25997]]
[[ID:26001]]
[[ID:26004]]

m1
m2
m3
"
  set_token("test token")
  expect_error(httptest::with_mock_api({
    (out <- suppressWarnings(as.qdoc.question(q.text)))
  }), NA)
})

test_that("qdoc creation", {
  data("qdocs")
  set_token("test token")
  httptest::with_mock_api({
    expect_error((qdoc <- suppressWarnings(read_qdoc(text = qdoc.simple))), NA)
    expect_error((qdoc <- suppressWarnings(read_qdoc(text = qdoc.adv))), NA)
  })
})
