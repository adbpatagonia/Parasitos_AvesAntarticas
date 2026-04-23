# setup script ----
source(paste0(here::here(), "/analysis/ST_0_LoadPackages.R"))


# read data -----
## map ----
antartida  <-  st_read(paste0(here::here(), "/data/map/ADD_Coastline_medium_res_polygon.shp"), quiet = TRUE)

## parasitos -----

parasitos <- openxlsx::read.xlsx(xlsxFile = paste0(here::here(), "/data/Base_Helmintos_Aves_Antarticas.xlsx"),
                                 sheet = 'Registros') %>%
  data.table()

# wrangle data ----
## map ----
antartida <- st_make_valid(antartida)

target_crs <- st_crs(antartida)

### crs -----
crs_pen <- paste0(
  "+proj=laea ",
  "+lat_0=-67 ",
  "+lon_0=-64 ",
  "+datum=WGS84 +units=m +no_defs"
)

### Antarctic Peninsula approximate window -----
# observed limits
# lon: -68.8833 to -44.5670
# lat: -68.1170 to -60.6830

pts_ll <- data.frame(
  lon = c(-68.8833, -44.5670),
  lat = c(-68.1170, -60.6830)
) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# transform limits into peninsula CRS
pts_pen <- st_transform(pts_ll, crs_pen)
xy <- st_coordinates(pts_pen)

# padding (km scale)
pad <- 150000

pen_bbox <- st_bbox(
  c(
    xmin = min(xy[,1]) - pad,
    xmax = max(xy[,1]) + pad,
    ymin = min(xy[,2]) - pad,
    ymax = max(xy[,2]) + pad
  ),
  crs = st_crs(crs_pen)
)

pen_box <- st_as_sfc(pen_bbox)


###  Keep land and CLIP to bbox ----
antartida <- st_transform(antartida, crs_pen)
ant_pen <- antartida %>%
  subset(SURFACE == "land") %>%
  st_intersection(pen_box)

#  Reproject so peninsula points north
# centered near Antarctic Peninsula

ant_pen <- st_transform(ant_pen, crs_pen)

## parasitos -----
# remove blank records
parasitos <- parasitos[!is.na(ID)]

# deal with column names
cnames <- tolower(make.names(colnames(parasitos)))

# clean names
cnames <- cnames |>
  iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>  # ñ -> n, á -> a, etc.
  gsub("\\.+$", "", x = _) |>                      # remove trailing dots
  gsub("\\.{2,}", ".", x = _)                      # multiple dots -> single dot

setnames(parasitos, cnames)

# set prevalencia as numeric
parasitos$prevalencia <- as.numeric(parasitos$prevalencia)

# standardize coordinate columns to numeric
parasitos[
  , `:=`(
    latitud.decimal  = as.numeric(latitud.decimal),
    longitud.decimal = as.numeric(longitud.decimal)
  )
]


# use decimal coords
parasitos_sf <- parasitos[
  !is.na(latitud.decimal) &
    !is.na(longitud.decimal)
] |>
  st_as_sf(
    coords = c("longitud.decimal", "latitud.decimal"),
    crs = 4326,      # WGS84 lon/lat
    remove = FALSE
  )

parasitos_sf <- st_transform(parasitos_sf, crs_pen)
pen_box <- st_transform(pen_box, crs_pen)

parasitos_sf_pen <- parasitos_sf %>%
  st_intersection(pen_box)


# plot ----

p.par.ant <- ggplot() +
  geom_sf(data = ant_pen,
          fill = "grey90",
          color = "black",
          linewidth = 0.2) +
  geom_sf(data = parasitos_sf_pen %>%
            filter(!isla.region %in% c(
              "Islas Crozet (Subantártico)" ,
              "Isla Macquarie (Subantártico)" ,
              "Antártida Oriental",
              "Islas Kerguelen (sub-Antártica)")),
          # size = 3,
          alpha = 0.5,
          aes(col = especie.parasita,
              # shape = ave.hospedadora.nombre.comun,
              size = prevalencia
              )) +
  # coord_sf(
  #   default_crs = st_crs(crs_pen),
  #   datum = crs_pen,
  #   expand = TRUE
  # ) +
  # geom_text_repel(
  #   data = parasitos_sf_pen %>%
  #     filter(!isla.region %in% c(
  #       "Islas Crozet (Subantártico)",
  #       "Isla Macquarie (Subantártico)",
  #       "Antártida Oriental",
  #       "Islas Kerguelen (sub-Antártica)"
  #     )),
  #   aes(label = especie.parasita),
  #   stat = "sf_coordinates"
  # ) +
  labs(
    x = "Easting",
    y = "Northing"
  ) +
  theme_minimal()


# output ----
ggsave(plot = p.par.ant, filename = paste0(here::here(), "/output/Mapa.png"),
       width = 13, height =8)
