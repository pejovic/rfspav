buffer.dist <- function(observations, predictionDomain, classes, width, ...){
  if(missing(width)){ width <- sqrt(areaSpatialGrid(predictionDomain)) }
  if(!length(classes)==length(observations)){ stop("Length of 'observations' and 'classes' does not match.") }
  ## remove classes without any points:
  xg = summary(classes, maxsum=length(levels(classes)))
  selg.levs = attr(xg, "names")[xg > 0]
  if(length(selg.levs)<length(levels(classes))){
    fclasses <- as.factor(classes)
    fclasses[which(!fclasses %in% selg.levs)] <- NA
    classes <- droplevels(fclasses)
  }
  ## derive buffer distances
  s <- list(NULL)
  for(i in 1:length(levels(classes))){
    s[[i]] <- raster::distance(rasterize(observations[which(classes==levels(classes)[i]),1]@coords, y=raster(predictionDomain)), width=width, ...)
  }
  s <- s[sapply(s, function(x){!is.null(x)})]
  s <- brick(s)
  s <- as(s, "SpatialPixelsDataFrame")
  s <- s[predictionDomain@grid.index,]
  return(s)
}


# ================= Preparing Meuse data =================================
library(sp)
demo(meuse, echo=FALSE)
meuse <- meuse[complete.cases(meuse@data),]

meuse.df <- as.data.frame(meuse)

set.seed(123)
meuse_split <- initial_split(meuse.df, prop = 0.70, strata = zinc)
meuse_train <- training(meuse_split)
meuse_test  <-  testing(meuse_split)

coordinates(meuse_train) <- ~x + y
crs(meuse_train) <- meuse@proj4string

coordinates(meuse_test) <- ~x + y
crs(meuse_test) <- meuse@proj4string


meuse_folds <- vfold_cv(meuse_train, v = 5)

meuse.fm <- as.formula("zinc ~ x + y + dist.m + soil + ffreq + landuse")


grid.dist0 <- buffer.dist(meuse_train["zinc"], meuse.grid[1], as.factor(1:nrow(meuse_train)))
dn0 <- paste(names(grid.dist0), collapse="+")
fm0 <- as.formula(paste("zinc ~ ", dn0))
fm0

ov.zinc <- over(meuse_train["zinc"], grid.dist0)
rm.zinc <- cbind(meuse_train@data["zinc"], ov.zinc)

m.zinc <- ranger(fm0, rm.zinc, quantreg=TRUE, num.trees=150, seed=1)
m.zinc

ovt.zinc <- over(meuse_test["zinc"], grid.dist0)
rmt.zinc <- cbind(meuse_test@data["zinc"], ovt.zinc)



rmt.zinc$pred <- predict(m.zinc, rmt.zinc)$predictions
str(rmt.zinc)

yardstick::rsq(rmt.zinc, truth = zinc, estimate = pred)

