test_that("has_choices works", {
  expect_equal(has_choices("ergerg[[Choices]]"), FALSE)
  expect_equal(has_choices("ergerg[[Choices]]"), FALSE)
  expect_equal(has_choices("ergerg[[Choices]]a"), TRUE)
  expect_equal(has_choices("ergerg[[Choices]]\na"), TRUE)
  expect_equal(has_choices("ergerg[[Choices]]1"), TRUE)
  expect_equal(has_choices("ergerg[[Choices]]\n1"), TRUE)
  expect_equal(has_choices("ergerg[[Choices]] "), FALSE)
  expect_equal(has_choices("ergerg[[Choices]]      "), FALSE)
  expect_equal(has_choices("ergerg[[Choices]]      \na"), TRUE)
  expect_equal(has_choices("ergerg[[AdvancedChoices]]      \na"), FALSE)
  expect_equal(has_choices("ergerg[[AdvancedChoices]]      \na[[Choice]]x"), TRUE)
  expect_equal(has_choices("ergerg[[AdvancedChoices]]      \na[[Choice:10]]x"), TRUE)
})

test_that("get_choices works", {
  expect_equal(get_choices("[[Choices]]a\naa\nb\nc\nd\n"),
               get_choices("[[Choices]]\na\naa\nb\nc\nd\n"))
  
  expect_error(get_choices("[[Choices]] "))
  expect_error(get_choices("[[Choices]]\n\n\n"))
  
  expect_equal(length(get_choices("\n\n[[Choices]]a\naa\nb\nc\nd\n")), 5)
})

test_that("get_rows works", {
  expect_equal(length(get_rows("\n\nStuff\n[[Choices]]\na\nb\nc\n[[AdvancedAnswers]]\n[[Answer]]\nanswer 1\n[[Answer]]\nanswer 2\n[[Answer]]\nanswer 3")), 3)
  expect_equal(length(get_rows("\n\nStuff\n\n[[Choices]]\na\nb\nc\n[[AdvancedCols]]\n[[Col]]\nanswer 1\n[[Col]]\nanswer 2\n[[Col]]\nanswer 3")), 3)
  expect_equal(length(get_rows("\n\nStuff\n\n[[Rows]]\na\nb\nc\n[[AdvancedAnswers]]\n[[Answer]]\nanswer 1\n[[Answer]]\nanswer 2\n[[Answer]]\nanswer 3")), 3)
  expect_equal(length(get_rows("\n\nStuff\n\n[[Choices]]\na\nb\nc\n[[Answers]]\na\nb\nc")), 3)
  expect_equal(length(get_rows("Info\nabout\nQuestion\n\na\nb\nc\n\n\n1\n2\n3")), 3)
})

test_that("get_cols works", {
  expect_equal(length(get_cols("\n\nStuff\n\n[[Choices]]\na\nb\nc\n[[Answers]]1\n2\n3")), 3)
  expect_equal(length(get_cols("\n\nStuff\n\n[[Choices]]\na\nb\nc\n[[AdvancedCols]]\n[[Col]]\n1\n[[Col]]\n2\n[[Col]]\n3")), 3)
  expect_equal(length(get_cols("\n\nStuff\n\n[[Rows]]\na\nb\nc\n[[AdvancedAnswers]]\n[[Answer]]\n1\n[[Answer]]\n2\n[[Answer]]\n3")), 3)
  expect_equal(length(get_cols("\n\nStuff\n\n[[Choices]]\na\nb\nc\n[[Answers]]\n1\n2\n3")), 3)
  expect_equal(length(get_cols("
Info
about
Question

a
b
c

1
2
3")), 3)
  
  expect_error(get_cols("Info\nabout\n\nQuestion\n\na\nb\nc\n\n\n1\n2\n3"))
  expect_error(get_cols("Info\nabout\n[[Rows]]\nQuestion\na\nb\nc\n\n\n1\n2\n3"))
})