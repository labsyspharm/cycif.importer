library(cycif.importer)

extdata <- system.file("extdata", package = "cycif.importer")

test_that("cycif_summarize aggregates by group", {
  data_dir <- file.path(extdata, "example_quants")
  roi_dir <- file.path(extdata, "example_rois")
  gate_file <- file.path(extdata, "example_gates.csv")
  cells <- cycif_load_cell_data(data_dir)
  cells <- purrr::map(cells, head, 30)
  rois <- cycif_load_roi_data(roi_dir)
  assigned <- cycif_assign_rois(cells, rois, expand_distance = 0)
  gates <- readr::read_csv(gate_file, show_col_types = FALSE)
  gated <- cycif_apply_gates(assigned, gates, double_gates = get_default_double_gates())
  sampled <- cycif_sample(gated, sample_size = NULL)
  summ <- cycif_summarize(sampled,
                         summary_filters = list(`CD8+` = ~CD8ap),
                         summary_groups = list(by_slide = "slideName"))
  expect_true("by_slide__CD8+" %in% names(summ))
  df <- summ[["by_slide__CD8+"]]
  expect_true(all(c("slideName", "cell_count") %in% names(df)))
  expect_true(nrow(df) >= 1)
})

test_that("cycif_summarize works with summary_filters = NULL", {
  data_dir <- file.path(extdata, "example_quants")
  roi_dir <- file.path(extdata, "example_rois")
  gate_file <- file.path(extdata, "example_gates.csv")
  cells <- cycif_load_cell_data(data_dir)
  cells <- purrr::map(cells, head, 30)
  rois <- cycif_load_roi_data(roi_dir)
  assigned <- cycif_assign_rois(cells, rois, expand_distance = 0)
  gates <- readr::read_csv(gate_file, show_col_types = FALSE)
  gated <- cycif_apply_gates(assigned, gates, double_gates = get_default_double_gates())
  sampled <- cycif_sample(gated, sample_size = NULL)

  # Test with NULL filters - should return unfiltered summaries
  summ <- cycif_summarize(sampled,
                         summary_filters = NULL,
                         summary_groups = list(
                           by_slide = "slideName",
                           by_roi = c("slideName", "ROI")
                         ))

  # Check that results are named by group only (no filter suffix)
  expect_true("by_slide" %in% names(summ))
  expect_true("by_roi" %in% names(summ))
  expect_false("by_slide__" %in% names(summ))  # No double underscore

  # Check that by_slide summary exists and has expected structure
  df_slide <- summ[["by_slide"]]
  expect_true(all(c("slideName", "cell_count") %in% names(df_slide)))
  expect_true(nrow(df_slide) >= 1)

  # Check that by_roi summary exists
  df_roi <- summ[["by_roi"]]
  expect_true(all(c("slideName", "ROI", "cell_count") %in% names(df_roi)))
  expect_true(nrow(df_roi) >= 1)

  # Verify that all cells are included (not filtered)
  # Count cells from the list-format sampled data
  if (is.data.frame(sampled)) {
    total_cells_input <- nrow(sampled)
  } else {
    total_cells_input <- sum(purrr::map_int(sampled, nrow))
  }
  total_cells_summary <- sum(df_slide$cell_count)
  expect_equal(total_cells_input, total_cells_summary)
})
