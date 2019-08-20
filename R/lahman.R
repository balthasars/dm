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

cdm_lahman <- nse_function(c(cycle = FALSE, color = TRUE), ~{

})

library(dm)
library(dplyr)
library(purrr)

# relations are not imported by dbplyr
# dm::cdm_learn_from_db(dbplyr::copy_lahman(dplyr::src_postgres()))

# lahman_dm_raw <- dplyr::src_df("Lahman") %>%
#   dm::as_dm()
# cannot remove tables from dm object
# thus verbose, as dbplyr:::lahman_tables
# also lists too many

# library(Lahman)

# alternative: use `src_df()` but how to `filter()`?

dm_lahman_no_keys <- list(
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
) %>%
  # create surrogate key
  map(~ mutate(., id = row_number()) %>% as_tibble()) %>%
  as_dm()

# map function that sets names in list — important for table names
map_named <- function(x, ...) map(x, ...) %>%
  set_names(x)

# get primary keys for all
all_pk_candidates <- dm_lahman_no_keys %>%
  cdm_get_tables() %>%
  attr("names") %>%
  # iterate over names
  map_named(cdm_enum_pk_candidates, dm = dm_lahman_no_keys) %>%
  tibble::enframe() %>%
  tidyr::unnest() %>%
  # only actual candidates
  filter(candidate) %>%
  group_by(name) %>%
  add_tally() %>%
  ungroup()

# get surrogate keys where no other keys have a natural fit,
# leave primary key where there is a natural fit
more_than_one_pk_candidate <- all_pk_candidates %>%
  filter(n > 1) %>%
  # more than two keys: randomly choose
  group_by(name) %>%
  sample_n(1) %>%
  ungroup

pk_candidates <- all_pk_candidates %>%
  filter(n == 1) %>%
  bind_rows(more_than_one_pk_candidate)

# check if all are covered
# pk_candidates %>% count(name) %>% filter(n > 1)
# obvs this doesn't work
# map2(
#    pk_candidates$name, pk_candidates$column,
#    dm = dm_lahman_no_keys
#    )
# recursive function beyond the scope of this exercise
# there must be a better way to do this!

# debug flawed map call
# differences in table names?
# setequal(names(cdm_get_tables(dm_lahman_no_keys)), pk_candidates$name)

# generate columns
# pk_candidates %>%
#   select(name, column) %>%
#   datapasta::tribble_paste()
dm_lahman_pk <- dm_lahman_no_keys %>%
  cdm_add_pk(AllstarFull, id) %>%
  cdm_add_pk(Appearances, id) %>%
  cdm_add_pk(AwardsManagers, id) %>%
  cdm_add_pk(AwardsPlayers, id) %>%
  cdm_add_pk(AwardsShareManagers, id) %>%
  cdm_add_pk(AwardsSharePlayers, id) %>%
  cdm_add_pk(Batting, id) %>%
  cdm_add_pk(BattingPost, id) %>%
  cdm_add_pk(CollegePlaying, id) %>%
  cdm_add_pk(Fielding, id) %>%
  cdm_add_pk(FieldingOF, id) %>%
  cdm_add_pk(FieldingPost, id) %>%
  cdm_add_pk(HallOfFame, id) %>%
  cdm_add_pk(Managers, id) %>%
  cdm_add_pk(ManagersHalf, id) %>%
  cdm_add_pk(Pitching, id) %>%
  cdm_add_pk(PitchingPost, id) %>%
  cdm_add_pk(Salaries, id) %>%
  cdm_add_pk(SeriesPost, id) %>%
  cdm_add_pk(Teams, id) %>%
  cdm_add_pk(TeamsHalf, id) %>%
  cdm_add_pk(Parks, park.key) %>%
  cdm_add_pk(People, id) %>%
  cdm_add_pk(Schools, schoolID) %>%
  cdm_add_pk(TeamsFranchises, id)

# add foreign keys

# get foreign keys of combinations
tbl_names <- cdm_get_tables(dm_lahman_pk) %>%
  attr("names")

# combinations
combs <- tidyr::crossing(tbl_names, tbl_names) %>%
  filter(tbl_names != tbl_names1)

<- map2_df(
  combs$tbl_names,
  combs$tbl_names1,
  cdm_enum_fk_candidates,
  dm = dm_lahman_pk
  )

# add
dm_lahman_pk
lahman_dm_raw_pk %>%
  cdm_add_fk(table = People, playerID, ref_table = Appearances) %>%
  # some managers were also players
  cdm_add_fk(AwardsManagers, playerID, People) #%>%
  # cdm_add_fk(People, Teams)





  # map_named(cdm_enum_pk_candidates, dm = lahman_dm_raw) %>%


lahman_dm_raw %>%
  cdm_

lahman_dm_raw %>%
  cdm_enum_fk_candidates(AwardsManagers, Managers)
