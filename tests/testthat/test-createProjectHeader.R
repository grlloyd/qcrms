context ("Test createProjectHeader")

expect_equal(qcrms:::createProjectHeader(testData$QCreportObject)$
    projectHeader, testData$createProjectHeader)
