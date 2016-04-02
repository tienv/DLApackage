context("DisplayArrLab")

test_that("DisplayArrLab give errors when needed",
          {
            x = 1:3
            expect_error(DisplayLabArr(x))
            y = array(1:12,c(2,3,2))
            bg = array(FALSE,c(2,2,2))
            expect_error(DisplayLabArr(label.array = y, bg.index = bg))
          })