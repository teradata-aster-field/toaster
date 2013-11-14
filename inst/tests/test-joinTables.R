context("joinTables")

test_that("different types of joins work", {
  
  expect_equal(joinTables("stores", "employees", join="inner", "s", "e", "id"),
               "( SELECT s.* FROM stores s INNER JOIN employees e ON ( s.id=e.id ) ) t")
              
  expect_equal(joinTables("stores", "employees", join="full", "s", "e", "id"),
               "( SELECT s.* FROM stores s FULL OUTER JOIN employees e ON ( s.id=e.id ) ) t")
  
  expect_equal(joinTables("stores", "employees", join="left", "s", "e", "id"),
               "( SELECT s.* FROM stores s LEFT JOIN employees e ON ( s.id=e.id ) ) t")

})

test_that("column lists in joins work", {
  
  expect_equal(joinTables("stores", "employees", join="inner", joinColumns=c("id", "region"),
                          select1=c("name","region","manager"), select2=c("fullname","uppermanager")),
               "( SELECT a.name, a.region, a.manager, b.fullname, b.uppermanager FROM stores a INNER JOIN employees b ON ( a.id=b.id AND a.region=b.region ) ) t")
})



