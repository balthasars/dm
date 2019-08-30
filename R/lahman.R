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

#  which packages are okay to use for building
# functions? get dependencies:
# itdepends::dep_usage_pkg("dm") %>% distinct(pkg)

# cdm_lahman <- nse_function(c(cycle = FALSE, color = TRUE), ~ {
#
# })

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

# <- dbplyr:::lahman_tables()
# lahman_src <- dbplyr::copy_lahman(src_postgres())
# lahman_dm <- lahman_src %>% dm::cdm_learn_from_db()

# get names of tables
names_src_lahman <- src_df(pkg = "Lahman") %>%
  .$env %>%
  objects()

# subset to relevant tables
sub_names_src <- setdiff(
  names_src_lahman,
  c(
    # remove deprecated dfs
    "Master", "LahmanData",
    # remove dfs with labels
    stringr::str_extract(names_src_lahman, ".*Label.*")
    )
)

# function takes name of data frame as input col `tibble_name` and evaluates it
# in other col `data` to produce list column
tibble_with_names <- function(input_tibble) {
  tibble(
    tibble_name = as.character(input_tibble),
    # https://stackoverflow.com/questions/1743698/evaluate-expression-given-as-a-string
    # see https://adv-r.hadley.nz/quasiquotation.html too, for {rlang} alternatives
    data = eval(parse(text = input_tibble)) %>% list()
  ) %>%
  # repurpose column to make name for `dm` later on
  mutate(tibble_name = stringr::str_remove(tibble_name, "Lahman::"))
}

# example for `tibble_with_names()`
# "Lahman::TeamsHalf" %>%
#   tibble_with_names()

data_nested <-
  # construct source names
  paste0("Lahman::", sub_names_src) %>%
  # make nested data frame
  map_df(tibble_with_names)

# lahman_no_keys %>% cdm_enum_pk_candidates("AwardsManagers")

# from nested df to list to dm
lahman_no_keys <- set_names(as.list(data_nested$data), data_nested$tibble_name) %>%
  as_dm()

# with `list2()` and splicing?
# make_list <- function(list_element_names, data_frames){
#   list_element_names_quos <- enquos(list_element_names)
#   data_frames_quos <- enquos(data_frames)
#
#   rlang::list2(list_element_name_quo := !!!data)
# }

get_all_pk_candidates <- function(dm_input) {

  # `map()` function that sets names in list — needed for table
  map_named <- function(x, ...) map(x, ...) %>%
      set_names(x)

  df_names <- cdm_get_tables(dm_input) %>%
    names()

  # get pk candidates for all tables
  candidates_all_tables <- df_names %>%
    map_named(cdm_enum_pk_candidates, dm = dm_input) %>%
    tibble::enframe() %>%
    tidyr::unnest()
}

candidates_all_tables <- get_all_pk_candidates(lahman_no_keys)

dfs_with_pk_candidates <- candidates_all_tables %>%
  filter(candidate) %>%
  distinct(name) %>%
  pull(name)

dfs_without_pk_candidates <- candidates_all_tables %>%
  filter(!candidate) %>%
  distinct(name) %>%
  pull(name) %>%
  setdiff(., dfs_with_pk_candidates)

#  get surrogate keys where no other keys have a natural fit,
#  leave primary key where there is a natural fit

pk_candidates_tallied <- candidates_all_tables %>%
  group_by(name) %>%
  filter(candidate) %>%
  add_tally()

more_than_one_pk_candidate <- pk_candidates_tallied %>%
  filter(n > 1) %>%
  # more than two keys: grouped randomly sample
  # maybe not such a good idea?
  sample_n(1) %>%
  ungroup()

pk_candidates <- pk_candidates_tallied %>%
  filter(n == 1) %>%
  bind_rows(more_than_one_pk_candidate) %>%
  select(-n)

no_pk_candidates <- dfs_without_pk_candidates

set_keys <- tibble(
  df = c(dfs_without_pk_candidates, pk_candidates$name),
  keys = c(rep_along(dfs_without_pk_candidates, NA), pk_candidates$column)
)

# add surrogate keys to dfs where no primary key is available

data_nested %>%
  mutate(data, )


# function to add surrogate key
add_id <- function(df, df_name) {
  df_name_quo <- paste0(df_name, "_id")
  df %>%
    mutate(!!df_name_quo := row_number())
}

# add_id(df = Lahman::AllstarFull, df_name = "hello") %>%
#   as_tibble()


# check if all are covered
# pk_candidates %>% count(name) %>% filter(n > 1)
# obvs this doesn't work
# map2(
#    pk_candidates$name, pk_candidates$column,
#    dm = dm_lahman_no_keys
#    )
#  recursive function beyond the scope of this exercise
# there must be a better way to do this!

#  debug flawed map call
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

#  get table names
table_names <- cdm_get_tables(dm_lahman_pk) %>%
  attr("names")

# make combinations of tables
table_combs <- tidyr::crossing(table_names, table_names) %>%
  filter(table_names != table_names1)

#  check candidates for each combination
# ---> this makes no sense with surrogate keys?
# combinations <- map2_df(
#   table_combs$table_names,
#   table_combs$table_names1,
#   cdm_enum_fk_candidates,
#   dm = dm_lahman_pk
# )
#
# combinations %>%
#   filter(candidate)

# add
dm_lahman_pk
lahman_dm_raw_pk %>%
  cdm_add_fk(table = People, playerID, ref_table = Appearances) %>%
  # some managers were also players
  cdm_add_fk(AwardsManagers, playerID, People) # %>%
# cdm_add_fk(People, Teams)


lahman_dm_raw %>%
  cdm_enum_fk_candidates(AwardsManagers, Managers)
