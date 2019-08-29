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

# <- dbplyr:::lahman_tables()
# lahman_src <- dbplyr::copy_lahman(src_postgres())
# lahman_dm <- lahman_src %>% dm::cdm_learn_from_db()

# get names of tables
names_src_lahman <- src_df(pkg = "Lahman") %>%
  .$env %>%
  objects()

# exclude deprecated data
setdiff(names_src_lahman, c("Master", "LahmanData")) %>%
  paste0("Lahman::", .)

tibble_with_names <- function(input_tibble){
  tibble(
    tibble_name = as.character(input_tibble),
    data = eval(parse(text = input_tibble)) %>% list()
  )
}

# example
# "Lahman::TeamsHalf" %>%
#   tibble_with_names()

# function to add primary key
add_id <- function(df, df_name){
  df_name_quo <- paste0(df_name, "_id")
  df %>%
    mutate(!!df_name_quo := row_number())
}

data_nested <-
  # exclude deprecated data
  setdiff(names_src_lahman, c("Master", "LahmanData")) %>%
  # construct source
  paste0("Lahman::", .) %>%
  # add all to nested data frame
  map_df(tibble_with_names) %>%
  # repurpose column to make list name
  mutate(tibble_name = str_remove(tibble_name, "Lahman::"))

# from nested df to list to dm
lahman_no_keys <- set_names(as.list(data_nested$data), data_nested$tibble_name) %>%
  as_dm()

# with `list2()` ?
# make_list <- function(list_element_names, data_frames){
#   list_element_names_quos <- enquos(list_element_names)
#   data_frames_quos <- enquos(data_frames)
#
#   rlang::list2(list_element_name_quo := !!!data)
# }

add_id(df = Lahman::AllstarFull, df_name = "hello") %>%
  as_tibble()

# map function that sets names in list — important for table names
map_named <- function(x, ...) map(x, ...) %>%
  set_names(x)

get_all_pks <- function(dm_input){
  table_names <- cdm_get_tables(dm_input) %>%
    names()
  table_names %>%
    map_named(cdm_enum_pk_candidates, dm = dm_input) %>%
    tibble::enframe() %>%
    tidyr::unnest() %>%
    # only actual candidates
    filter(candidate) %>%
    group_by(name) %>%
    add_tally() %>%
    ungroup()
}

all_pk_candidates <- get_all_pks(lahman_no_keys)

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

# get table names
table_names <- cdm_get_tables(dm_lahman_pk) %>%
  attr("names")

# make combinations of tables
table_combs <- tidyr::crossing(table_names, table_names) %>%
  filter(table_names != table_names1)

# check candidates for each combination
combinations <- map2_df(
  table_combs$table_names,
  table_combs$table_names1,
  cdm_enum_fk_candidates,
  dm = dm_lahman_pk
)

combinations %>%
  filter(candidate)

# add
dm_lahman_pk
lahman_dm_raw_pk %>%
  cdm_add_fk(table = People, playerID, ref_table = Appearances) %>%
  # some managers were also players
  cdm_add_fk(AwardsManagers, playerID, People) #%>%
# cdm_add_fk(People, Teams)


lahman_dm_raw %>%
  cdm_enum_fk_candidates(AwardsManagers, Managers)
