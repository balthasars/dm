#' Creates a test [`dm`] object from \pkg{Lahman}
#'
#' @description Creates an exemplary [`dm`] object from the tables in \pkg{Lahman}
#' along with key relations.
#'
#' @param cycle Boolean. If `FALSE` (default), only one foreign key relation
#'   (from `flights$origin` to `airports$faa`) between `flights` and `airports` is
#'   established. If `TRUE`, a `dm` object with a double reference
#'   between those tables will be produced.
#' @param color Boolean, if `TRUE` (default), the resulting `dm` object will have
#'   colors assigned to different tables for visualization with `cdm_draw()`
#'
#' @export
cdm_nycflights13 <- function(cycle = FALSE, color = TRUE) h(~ {
    dm <-
      dm(
        src_df("nycflights13")
      ) %>%
      cdm_add_pk(planes, tailnum) %>%
      cdm_add_pk(airlines, carrier) %>%
      cdm_add_pk(airports, faa) %>%
      cdm_add_fk(flights, tailnum, planes, check = FALSE) %>%
      cdm_add_fk(flights, carrier, airlines) %>%
      cdm_add_fk(flights, origin, airports)

    if (color) {
      dm <-
        dm %>%
        cdm_set_colors(
          flights = "blue",
          airports = ,
          planes = ,
          airlines = "orange",
          weather = "green"
        )
    }

    if (cycle) {
      dm %>%
        cdm_add_fk(flights, dest, airports, check = FALSE)
    } else {
      dm
    }
  })

cdm_lahman <- function(color = TRUE){

}

library(dplyr)
library(dm)

# dm::cdm_learn_from_db(dbplyr::copy_lahman(dplyr::src_postgres()))

# lahman_dm_raw <- dplyr::src_df("Lahman") %>%
#   dm::as_dm()
# cannot remove tables from dm object
# thus verbose, as dbplyr:::lahman_tables
# also lists too many


library(Lahman)
lahman_dm_raw <- as_dm(
  list(
    "AllstarFull"
    "Appearances"
    "AwardsManagers"
    "AwardsPlayers"
    "AwardsShareManagers"
    "AwardsSharePlayers"
    "Batting"
    "BattingPost"
    "CollegePlaying"
    "Fielding"
    "FieldingOF"
    "FieldingPost"
    "HallOfFame"
    # LahmanData, # meta table
    "Managers",
    "ManagersHalf",
    # Master, # deprecated
    "Parks",
    "People",
    "Pitching",
    "PitchingPost",
    "Salaries",
    "Schools",
    "SeriesPost",
    "Teams",
    "TeamsFranchises",
    "TeamsHalf"
  )
)

# which packages are okay to use for building
# functions? get dependencies:
# itdepends::dep_usage_pkg("dm") %>% distinct(pkg)


lahman_dm_raw %>%
  cdm_enum_fk_candidates(AwardsManagers, Managers)

lahman_dm_raw %>%
  map(cdm_enum_fk_candidates)

map_named <- function(x, ...) map(x, ...) %>%
  set_names(x)

lahman_dm_raw %>%
  cdm_get_tables() %>%
  attr("names") %>%
  map_named(cdm_enum_pk_candidates, dm = lahman_dm_raw) %>%
  enframe() %>%
  unnest() %>%
  filter(candidate)


lahman_dm_raw %>%
  cdm_add_pk(People, playerID) %>%
  cdm_add_pk()

# People: players
