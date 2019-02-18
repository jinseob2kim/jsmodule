app <- ShinyDriver$new("../")
app$snapshotInit("basicTest")

app$snapshot(screenshot = FALSE)
app$setInputs(check_subset = TRUE)
