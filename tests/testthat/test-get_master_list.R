test_that("get_master_list() correctly imports Master List", {
  df <- suppressWarnings(get_master_list())
  expect_equal(nrow(df), 1644)
  expect_equal(
    colnames(df),
    c(
      "ingredient", "brand_name", "likelihood_score", "chapter_title",
      "last_update", "year_approved", "drug", "hds",
      "ns", "min", "type_of_agent", "in_liver_tox",
      "primary_classification", "secondary_classification",
      "tertiary_classification", "fatal", "rechall", "number_cases",
      "comments_a", "comments_b"
    )
  )
})
