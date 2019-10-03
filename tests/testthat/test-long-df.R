

## units, regions, years
# regions
## expect_equal(
##     unique(function$region),
##     unique(input$region)
## ## )

## # years
## expect_equal(
##     unique(function$time),
##     unique(input$time)
## )

## # unit
## expect_equal(
##     unique(function$unit),
##     unique(input$unit)
## )

## ## number of observation, region + time ----------------------------------------
## function %>%
## group_by(region, time) %>%
## summarise(
##     n_obs = tally()
##     )

## input %>%
## group_by(region, time) %>%
## summarise(
##     n_obs = tally()
## )

## ## number of observation, unit + time ------------------------------------------
## function %>%
## group_by(unit, time) %>%
## summarise(
##     n_obs = tally()
## )

## input %>%
## group_by(unit, time) %>%
## summarise(
##     n_obs = tally()
## )

## ## correct variables in input --------------------------------------------------
## c("time, region, unit") %in% names(input)
## c("time, region, unit") %in% names(output)
## 
