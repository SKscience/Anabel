# Copyright (C) 2025  Stefan Kraemer
#   
#   This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

get_local_package_version = function(package_name){
  local_version <- as.character(packageVersion(package_name))
  return(local_version)
}

check_and_update_package_from_cran = function(package_name, local_version){
  # First check CRAN version
  available_pkgs <- available.packages()
  if (package_name %in% rownames(available_pkgs)) {
    cran_version <- available_pkgs[package_name, "Version"]
    
    if (package_version(cran_version) > package_version(local_version)) {
      message(paste0("Your installed version of ", package_name, " is: ", 
                   local_version, 
                   "\nThere is a newer verion on CRAN: ", 
                   cran_version))
      conf_github <- readline("Do you want to update? (Y/y): ")
      if (conf_anabel == "Y" | conf_anabel == "y") {
        install.packages(package_name, dependencies = TRUE)
      }
    }
  }
}

check_and_update_package_from_github = function(github_repo, package_name, local_version){
  
  # Check GitHub version using a different approach
  # Get the GitHub release information
  github_release <- remotes::parse_github_repo_spec(github_repo)
  gh_user <- github_release$username
  gh_repo <- github_release$repo
  
  # Get latest release information
  release_data <- httr::GET(
    paste0("https://api.github.com/repos/", gh_user, "/", gh_repo, "/releases/latest")
  )
  if (httr::status_code(release_data) == 200) {
    release_info <- httr::content(release_data)
    github_version <- gsub("^v", "", release_info$tag_name)
    # Compare with local version (which might have been updated from CRAN)
    if (package_version(github_version) > package_version(local_version)) {
      message(paste0("Your installed version of ", package_name, " is: ", 
                   local_version, 
                   "\nThere is a newer verion on GitHub: ", 
                   github_version))
      conf_github <- readline("Do you want to update? (Y/y): ")
      if (conf_github == "Y" | conf_github == "y") {
        devtools::install_github(github_repo)
      }
    } else {
      message("Your 'anabel' package is up to date with the latest GitHub release.")
    }
  } else {
    message("Could not retrieve GitHub release information.")
  }
}


check_and_install_packages = function(list_of_packages){
  # Identify missing packages
  new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  # Ask user before installing missing packages
  if (length(new.packages)) {
    message(paste0('The following packages are missing:\n',  
                   paste(new.packages, collapse = "\n")))
    conf <- readline(prompt='Do you want to install them? (Y/y): ')
    if (conf == "Y" | conf == "y") {
      install.packages(new.packages, dependencies = TRUE)
    } else {
      stop(paste0("App start aborted. Missing packages:\n", paste(new.packages, collapse = "\n")))
    }
  }
  
  # Since the first block would have installed anabel if it was missing,
  # we can now assume anabel is installed and just check for updates
  local_version <- get_local_package_version("anabel")
  
  check_and_update_package_from_cran("anabel", local_version)
  local_version <- get_local_package_version("anabel")
  
  check_and_update_package_from_github(github_repo = 'SKscience/anabel_backend', 'anabel', local_version)
  

}

load_all_packages = function(list_of_packages){
  # Load all packages (excluding remotes which was just for installation)
  lapply(setdiff(list_of_packages, "remotes"), library, character.only = TRUE)
}