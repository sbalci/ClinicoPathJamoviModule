# devtools::install_github("edgararuiz/ecodown")

library(ecodown)

repo_location <- ecodown_clone(
  repo_url = "https://github.com/sbalci/meddecide",
  branch = "master"
  )

ecodown_convert(repo_location, quarto_sub_folder = "meddecide", branch = "master")

ecodown_quarto_render(quarto_folder = "~/meddecide")

ecodown_autolink(quarto_folder = "~/meddecide")
