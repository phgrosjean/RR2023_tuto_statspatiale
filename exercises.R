# Exercices RR2023 stats spatiales

## Exercice 1

### Question 1

library(sf)
iris75 <- st_read("exercises/data/iris_75.shp")

### Question 2

plot(iris75) # 2 graphiques

### Question 3

st_geometry(iris75) # Récupère uniquement la colonne geometry
plot(st_geometry(iris75))

### Question 4

accidents2019 <- st_read("exercises/data/accidents2019_paris.geojson")
plot(accidents2019) # Affiche les 9 preières variables, mais pas les frontières

# Solution : d'abord afficher les frontières à partir d'iris75, puis ajouter la carte des accidents avec add = TRUE
plot(st_geometry(iris75), bg = "cornsilk", col = "lightblue", 
  border = "white", lwd = .5)
plot(st_geometry(accidents2019), col = "red", pch = 20, cex = .2, add = TRUE)
title("Accidents à Paris")

### Question 5

iris75_union <- st_union(iris75)
st_buffer(iris75_union, dist = 1000) |>
  plot(border = "red")
plot(st_geometry(iris75), add = TRUE)
iris75_cent <- st_centroid(st_geometry(iris75))
plot(iris75_cent, pch = 20, cex = 0.6, col = "red", add = TRUE)
# Distances entre centroïdes (il faut x et y)
iris75_centdist <- st_distance(x = iris75_cent, y = iris75_cent)
# On a un objet de classe 'units' !
# Polygones de Voronoï
iris75voro <- st_voronoi(st_union(iris75_cent))
plot(iris75voro, border = 'blue', col = NA, add = TRUE)

# ... à finir!


## Exercice 2


library(dplyr)
system.time(acc_iris <- iris75 |>
  st_join(accidents2019) |> 
  group_by(CODE_IRIS) |>
  dplyr::summarize(
    nb_acc      = n(),
    nb_acc_grav = sum(if_else(grav %in% c(2, 3), 1, 0), na.rm = TRUE),
    nb_vl       = sum(if_else(catv == "VL seul", 1, 0), na.rm = TRUE),
    nb_edp      = sum(if_else(catv == "EDP à moteur", 1, 0), na.rm = TRUE),
    nb_velo     = sum(if_else(catv == "Bicyclette", 1, 0), na.rm = TRUE)
  ) 
) # 15,5sec
head(acc_iris, 1)

system.time(acc_iris2 <- iris75 |>
  st_join(accidents2019) |> 
  group_by(CODE_IRIS) |>
  dplyr::summarize(
    nb_acc      = n(),
    nb_acc_grav = sum(grav == 2 | grav == 3, na.rm = TRUE),
    nb_vl       = sum(catv == "VL seul", na.rm = TRUE),
    nb_edp      = sum(catv == "EDP à moteur", na.rm = TRUE),
    nb_velo     = sum(catv == "Bicyclette", na.rm = TRUE)
  )
) # 15,2sec
head(acc_iris2, 1)
waldo::compare(acc_iris, acc_iris2) # doubles dans acc_iris et entiers dans acc_iris2

library(collapse)
system.time(acc_iris3 <- iris75 |>
  st_join(accidents2019) |> 
  fgroup_by(CODE_IRIS) |>
  fsummarize(
    nb_acc      = fnobs(grav),
    nb_acc_grav = fsum(grav == 2 | grav == 3, na.rm = TRUE),
    nb_vl       = fsum(catv == "VL seul", na.rm = TRUE),
    nb_edp      = fsum(catv == "EDP à moteur", na.rm = TRUE),
    nb_velo     = fsum(catv == "Bicyclette", na.rm = TRUE),
    geometry    = st_union(geometry)
  )
) # 14,8sec
head(acc_iris3, 1)
waldo::compare(acc_iris2, acc_iris3)
# Attribut agr non recalculé avec collapse (de même que geometry, autoatiquement)
# class = c("sf", "tbl_df", "tbl", "data.frame") vs c("sf", "data.frame")
# pour nb_acc, collapse sometimes returns 0 where dplyr returns 1