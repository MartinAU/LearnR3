## code to prepare `nurses-stress` dataset goes here

library(here)

# The dataset can be downloaded manually from
# <https://drive.proton.me/urls/ZHZ6TCAN2W#e5D5U4IajdYp>.
# Make sure to save this file to the `data-raw/` folder with the name
# `nurses-stress.tar`.

untar(
  file.path("data-raw/nurses-stress.tar"),
  exdir = file.path("data-raw/nurses-stress/")
)

