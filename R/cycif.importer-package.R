#' @importFrom dplyr filter select mutate group_by summarize bind_rows
#' @importFrom dplyr across any_of where left_join n distinct everything
#' @importFrom stats complete.cases
NULL

# Column names used unquoted in dplyr / tidygraph pipelines
utils::globalVariables(c(
  ".tidygraph_edge_index", "CellID", "ROI", "ROIname", "Xt", "Yt",
  "area_inputs", "area_intersection", "destination", "edge", "geometry",
  "id", "issue", "origins", "proportions_input", "reason", "slideName"
))
