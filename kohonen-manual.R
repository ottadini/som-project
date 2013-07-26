# ===== Examples from kohonen-manual.pdf (Wehrens & Buydens 2007, J Stat Soft)
data("nir")
attach(nir)
set.seed(13)
nir.xyf <- xyf(data = spectra,
               Y = composition[,2],
               xweight = 0.5,
               grid = somgrid(6, 6, "hexagonal"))
par(mfrow = c(1, 2))
plot(nir.xyf, type = "counts", main = "NIR data: counts")
plot(nir.xyf, type = "quality", main = "NIR data: mapping quality")

set.seed(13)
nir.xyf2 <- xyf(data = spectra, Y = classvec2classmat(temperature), xweight = .2, grid = somgrid(6, 6, "hexagonal"))

water.predict <- predict(nir.xyf)$unit.prediction
temp.xyf <- predict(nir.xyf2)$unit.prediction
temp.predict <- as.numeric(classmat2classvec(temp.xyf))

par(mfrow = c(1, 2))
plot(nir.xyf, type = "property", property = water.predict,
     main = "Prediction of water content", keepMargins = TRUE)
scatter <- matrix(rnorm(length(temperature)*2, sd=.1), ncol=2)
radii <- (temperature - 20)/250
symbols(nir.xyf$grid$pts[nir.xyf$unit.classif,] + scatter, 
        circles = radii, inches = FALSE, add = TRUE)
plot(nir.xyf2, type = "property", property = temp.predict, 
     palette.name = rainbow, main = "Prediction of temperature")

radii <- 0.05 + 0.4 * composition[,2]
symbols(nir.xyf2$grid$pts[nir.xyf2$unit.classif,] + scatter, 
        circles = radii, inches = FALSE, add = TRUE)

par(mfrow = c(1, 2))
plot(nir.xyf2, "codes")
