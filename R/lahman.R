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

# which packages are okay to use for building
# functions? get dependencies:
# itdepends::dep_usage_pkg("dm") %>% distinct(pkg)

cdm_lahman <- function(color = TRUE){

}

library(dplyr)
library(dm)

# relations are not imported by dbplyr
# dm::cdm_learn_from_db(dbplyr::copy_lahman(dplyr::src_postgres()))

# lahman_dm_raw <- dplyr::src_df("Lahman") %>%
#   dm::as_dm()
# cannot remove tables from dm object
# thus verbose, as dbplyr:::lahman_tables
# also lists too many


# library(Lahman)
lahman_dm_raw <- as_dm(
  list(
    "AllstarFull" = Lahman::AllstarFull,
    "Appearances" = Lahman::Appearances,
    "AwardsManagers" = Lahman::AwardsManagers,
    "AwardsPlayers" = Lahman::AwardsPlayers,
    "AwardsShareManagers" = Lahman::AwardsShareManagers,
    "AwardsSharePlayers" = Lahman::AwardsSharePlayers,
    "Batting" = Lahman::Batting,
    "BattingPost" = Lahman::BattingPost,
    "CollegePlaying" = Lahman::CollegePlaying,
    "Fielding" = Lahman::Fielding,
    "FieldingOF" = Lahman::FieldingOF,
    "FieldingPost" = Lahman::FieldingPost,
    "HallOfFame" = Lahman::HallOfFame,
    # LahmanData, # meta table
    "Managers" = Lahman::Managers,
    "ManagersHalf" = Lahman::ManagersHalf,
    # Master, # deprecated
    "Parks" = Lahman::Parks,
    "People" = Lahman::People, # players
    "Pitching" = Lahman::Pitching,
    "PitchingPost" = Lahman::PitchingPost,
    "Salaries" = Lahman::Salaries,
    "Schools" = Lahman::Schools,
    "SeriesPost" = Lahman::SeriesPost,
    "Teams" = Lahman::Teams,
    "TeamsFranchises" = Lahman::TeamsFranchises,
    "TeamsHalf" = Lahman::TeamsHalf
  )
)

# get primary keys for all
map_named <- function(x, ...) map(x, ...) %>%
  set_names(x)

lahman_dm_raw %>%
  cdm_get_tables() %>%
  attr("names") %>%
  map_named(cdm_enum_pk_candidates, dm = lahman_dm_raw) %>%
  enframe() %>%
  unnest() %>%
  filter(candidate)

# intermediate setting of primary keys
lahman_dm_raw_pk <- lahman_dm_raw %>%
  # cols, where `cdm_enum_pk_candidates` finds candidates
  cdm_add_pk(People, playerID) %>%
  cdm_add_pk(Parks, park.key) %>%
  cdm_add_pk(Schools, schoolID) %>%
  cdm_add_pk(TeamsFranchises, franchID) %>%
  # cols where pk is set manually
  cdm_add_pk(AllstarFull, playerID)

lahman_dm_raw_pk %>%
  cdm_add_fk(table = People, playerID, ref_table = Appearances) %>%
  # some managers were also players
  cdm_add_fk(AwardsManagers, playerID, People) #%>%
  # cdm_add_fk(People, Teams)


# get foreign keys of combinations
tbl_names <- lahman_dm_raw %>%
  cdm_get_tables() %>%
  attr("names")

combs <- crossing(tbl_names, tbl_names) %>%
  filter(tbl_names != tbl_names1)

map2(combs$tbl_names, combs$tbl_names1, cdm_enum_fk_candidates, dm = lahman_dm_raw)

  # map_named(cdm_enum_pk_candidates, dm = lahman_dm_raw) %>%


lahman_dm_raw %>%
  cdm_

lahman_dm_raw %>%
  cdm_enum_fk_candidates(AwardsManagers, Managers)
