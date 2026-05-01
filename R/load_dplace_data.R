#' Load and wrangle data from D-PLACE
#'
#' Load and wrangle data on egalitarianism and other variables from the Standard
#' Cross-Cultural Sample via the online database D-PLACE (release v3.3.0). The
#' resulting dataset contains 181 societies that can be linked to the
#' phylogenetic tree.
#'
#' @details The dataset produced by this function is a tibble with 181
#'   observations and 24 variables:
#' \describe{
#'  \item{soc_id}{Character, society ID}
#'  \item{xd_id}{Character, cross-dataset ID (see
#'    https://d-place.org/glossary#q9)}
#'  \item{society}{Name of the society}
#'  \item{glottocode}{Glottocode for the language or dialect of the society}
#'  \item{language_family}{Language family for the language or dialect of the
#'    society}
#'  \item{region}{Region of the society}
#'  \item{focal_year}{Principal year to which data refer}
#'  \item{latitude}{Latitude of the society}
#'  \item{longitude}{Longitude of the society}
#'  \item{egalitarianism}{Factor, presence/absence of egalitarianism, defined as
#'    an absence of class-based or wealth-based distinctions in the society;
#'    coded from SCCS270}
#'  \item{percent_hunting}{Ordered factor (ten levels), percentage dependence on
#'    hunting; coded from SCCS204}
#'  \item{large_game_hunting}{Factor, presence/absence of large game hunting;
#'    coded from SCCS10}
#'  \item{food_sharing}{Ordered factor (seven levels), extent of food sharing;
#'    coded from SCCS1718}
#'  \item{food_storage}{Ordered factor (five levels), extent of food storage;
#'    coded from SCCS20}
#'  \item{starvation_occurrence}{Ordered factor (three levels), occurrence of
#'    short-term starvation; coded from SCCS1262}
#'  \item{famine_occurrence}{Ordered factor (four levels), occurrence of famine;
#'    coded from SCCS1265}
#'  \item{resource_problems}{Ordered factor (four levels), extent of chronic
#'    resource problems; coded from SCCS1685}
#'  \item{gossip_government}{Factor, presence/absence of gossip on government or
#'    colonial affairs; coded from SCCS1789}
#'  \item{gossip_politics}{Factor, presence/absence of gossip on politics; coded
#'    from SCCS1796}
#'  \item{gossip_social_control}{Factor, presence/absence of gossip on social
#'    control; coded from SCCS1799}
#'  \item{checks_power}{Ordered factor (four levels), extent of checks on
#'    leaders' power; coded from SCCS761}
#'  \item{remove_leaders}{Ordered factor (four levels), extent of institutions
#'    for removing leaders who are incompetent or disliked; coded from SCCS762}
#'  \item{political_fission}{Ordered factor (three levels), extent of local
#'    political fission of dissatisfied persons; coded from SCCS785}
#'  \item{political_violence}{Ordered factor (three levels), extent of violence
#'    against overarching political institution; coded from SCCS1739}
#' }
#'
#' @param dplace_data_url URL to access cldf/data.csv from D-PLACE v3.3.0
#' @param dplace_societies_url URL to access cldf/societies.csv from D-PLACE
#'   v3.3.0
#' @param glottolog_languages_url URL to access cldf/languages.csv from
#'   Glottolog v5.3
#' @param mcc_tree Maximum clade credibility tree of D-PLACE societies used to
#'   filter the dataset
#'
#' @returns A tibble
#'
load_dplace_data <- function(dplace_data_url, dplace_societies_url,
                             glottolog_languages_url, mcc_tree) {
  # load csv files
  data <- read.csv(file = dplace_data_url)
  societies <- read.csv(file = dplace_societies_url)
  languages <- read.csv(file = glottolog_languages_url)
  # ordered levels
  levels_SCCS204 <- c("0-5%", "6-15%", "16-25%", "26-35%", "36-45%", "46-55%",
                      "56-65%", "66-75%", "76-85%", "86-100%")
  levels_SCCS1718 <- c(
    "Sharing of food among nuclear family",
    "Sharing of food among kin residing in local community",
    "Sharing of food among kin, not restricted to local community",
    "Sharing of food among non-kin within local community",
    "Sharing of food among all members of local community",
    paste0(
      "Sharing of food among groups within unit of maximal ",
      "political authority or ethnic group"
    ),
    "Sharing of food among other than mentioned groups"
  )
  levels_SCCS20 <- c("None", "Individual households", "Communal facilities",
                     "Political agent controlled repositories",
                     "Economic agent controlled repositories")
  levels_SCCS1262 <- c("Very low", "Low", "High")
  levels_SCCS1265 <- c("Very low", "Low", "High", "Very high")
  levels_SCCS1685 <- c(
    "Low or rare (original code 1)" = "Low or rare",
    "original code 1.5"             = "Low or rare",
    'There are some "hungry times" during the year food is scarce' =
      "There are some hungry times",
    "Some members of the population do not have enough to eat" =
      "Some members of the population do not have enough to eat",
    "Most members of the pop. usually do not have enough to eat" =
      "Most members of the population usually do not have enough to eat",
    "Don't know (original code 8)" = NA,
    "No resolved rating (original code 0)" = NA
  )
  levels_SCCS761 <- c("Few", "Checks exist", "Leaders secure support",
                      "No leaders act independently")
  levels_SCCS762 <- c(
    "No way other than rebellion or popular uprisings",
    "Institutionalized means invoked occasionally, by elites",
    "Not removed in formal manner, lose influence & are ignored",
    "No formal leadership, loss of power when support diminishes"
  )
  levels_SCCS785 <- c("Rarely or never", "Sometimes",
                      "Often move to another community")
  levels_SCCS1739 <- c(
    "Violent acts absent",
    paste0(
      "Acts of violence, in reaction against attacks ",
      "by overarching political unit"
    ),
    "Active resistance, aiming at revolution"
  )
  # binary categories
  binary_SCCS270 <- c(
    "Absence of distinctions" = "Present",
    "Wealth distinctions"     = "Absent",
    "Elite stratification"    = "Absent",
    "Dual stratification"     = "Absent",
    "Complex stratification"  = "Absent"
  )
  binary_SCCS10 <- c(
    "Birds or Waterfowl"   = "Absent",
    "Small Mammals"        = "Absent",
    "Large Game"           = "Present",
    "Two or more of above" = "Absent"
  )
  binary_gossip <- c(
    "Absent"                = "Absent",
    "Present"               = "Present",
    "Present, Females Only" = "Present",
    "Present, Males Only"   = "Present"
  )
  # wrangle sccs data
  data |>
    # filter to sccs data only
    left_join(societies, by = c("Soc_ID" = "ID")) |>
    filter(Contribution_ID == "dplace-dataset-sccs") |>
    # pivot wider
    pivot_wider(
      id_cols = c(Soc_ID, xd_id, Name, Latitude, Longitude, region,
                  Glottocode, main_focal_year),
      names_from = Var_ID,
      values_from = Value
    ) |>
    # retain variables
    transmute(
      soc_id                = Soc_ID,
      xd_id                 = xd_id,
      society               = Name,
      glottocode            = Glottocode,
      region                = region,
      focal_year            = main_focal_year,
      latitude              = Latitude,
      longitude             = Longitude,
      egalitarianism        = ifelse(SCCS270 == "", NA,
                                     binary_SCCS270[SCCS270]),
      percent_hunting       = ordered(SCCS204, levels = levels_SCCS204),
      large_game_hunting    = ifelse(SCCS10 == "", NA, binary_SCCS10[SCCS10]),
      food_sharing          = ordered(str_to_sentence(SCCS1718),
                                     levels = levels_SCCS1718),
      food_storage          = ordered(SCCS20, levels = levels_SCCS20),
      starvation_occurrence = ordered(str_to_sentence(SCCS1262),
                                      levels = levels_SCCS1262),
      famine_occurrence     = ordered(str_to_sentence(SCCS1265),
                                      levels = levels_SCCS1265),
      resource_problems     = ordered(levels_SCCS1685[SCCS1685],
                                      levels = unique(levels_SCCS1685)[1:4]),
      gossip_government     = ifelse(SCCS1789 == "", NA,
                                     binary_gossip[SCCS1789]),
      gossip_politics       = ifelse(SCCS1796 == "", NA,
                                     binary_gossip[SCCS1796]),
      gossip_social_control = ifelse(SCCS1799 == "", NA,
                                     binary_gossip[SCCS1799]),
      checks_power          = ordered(SCCS761, levels = levels_SCCS761),
      remove_leaders        = ordered(SCCS762, levels = levels_SCCS762),
      political_fission     = ordered(SCCS785, levels = levels_SCCS785),
      political_violence    = ordered(str_to_sentence(SCCS1739),
                                      levels = levels_SCCS1739)
    ) |>
    # absent/present as factor
    mutate(
      across(
        where(is.character) & !c(soc_id, xd_id, society, glottocode, region),
        function(x) factor(x, levels = c("Absent", "Present"))
      )
    ) |>
    # filter to societies in phylogenetic tree (n = 181)
    filter(xd_id %in% mcc_tree$tip.label) |>
    # add data on language family affiliation
    left_join(
      dplyr::select(languages, Glottocode, Family_ID),
      by = c("glottocode" = "Glottocode")
    ) |>
    mutate(Family_ID = ifelse(Family_ID == "", NA, Family_ID)) |>
    left_join(
      dplyr::select(languages, Glottocode, Name),
      by = c("Family_ID" = "Glottocode")
    ) |>
    dplyr::select(soc_id:glottocode, Name, region:political_violence) |>
    rename(language_family = Name)
}
