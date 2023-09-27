//! Implements an overview graph.

use std::ops::RangeInclusive;

use eframe::egui::{self, Color32};
use ordered_float::NotNan;

use crate::file::{FileScoreCache, Files};

/// Filters which files to show.
#[derive(Debug)]
pub(crate) struct FileFilter {
    /// The minimum match score of files to show.
    min_score: Option<f64>,
    /// The maximum match score of files to show.
    max_score: Option<f64>,
    /// The height of this file filter.
    height: f32,
}

/// The background color of the graph.
const BACKGROUND_COLOR: Color32 = Color32::BLACK;

impl FileFilter {
    /// Creates a new file filter with default settings.
    pub(crate) fn new() -> Self {
        FileFilter {
            min_score: None,
            max_score: Some(0.05),
            height: 75.0,
        }
    }

    /// Draws the file filter.
    pub(crate) fn draw(
        &mut self,
        ui: &mut egui::Ui,
        files: &Files,
        file_score_cache: &FileScoreCache,
    ) {
        self.draw_sliders(ui);

        let size = egui::vec2(ui.available_width(), self.height);
        let (rect, response) = ui.allocate_exact_size(size, egui::Sense::click_and_drag());

        if response.dragged() {
            self.height = (self.height + response.drag_delta().y).clamp(10.0, f32::INFINITY);
        }

        ui.painter_at(rect).rect_filled(rect, 0.0, BACKGROUND_COLOR);

        let mut scores = files
            .chronological_order()
            .map(|file| {
                file_score_cache
                    .get_score(file.file_id)
                    .flatten()
                    .map(|score| NotNan::new(score).unwrap())
            })
            .collect::<Vec<_>>();

        scores.sort_unstable();

        let min = scores
            .iter()
            .position(|&score| score.map(f64::from) >= self.min_score)
            .unwrap_or(0);
        let max = scores
            .iter()
            .position(|&score| score.map(f64::from) > self.max_score)
            .unwrap_or(scores.len());

        self.draw_graph(ui, rect, &scores[min..max]);
    }

    /// Draws sliders to manipulate the minimum and maximum score.
    fn draw_sliders(&mut self, ui: &mut egui::Ui) {
        let width = ui.available_width() - 235.0;
        ui.horizontal(|ui| {
            use egui::Widget as _;

            let mut draw_slider =
                |score: &mut Option<f64>, text: &str, range: RangeInclusive<f64>| {
                    let mut temp_score = score.unwrap_or(0.0) * 100.0;

                    ui.horizontal(|ui| {
                        ui.spacing_mut().slider_width = width / 2.0;
                        ui.visuals_mut().widgets.inactive.fg_stroke =
                            egui::Stroke::new(1.0, super::utils::score_color(*score));
                        ui.visuals_mut().widgets.hovered.fg_stroke =
                            egui::Stroke::new(2.0, super::utils::score_color(*score));
                        ui.visuals_mut().widgets.active.fg_stroke =
                            egui::Stroke::new(3.0, super::utils::score_color(*score));

                        let response = egui::Slider::new(&mut temp_score, range)
                            .logarithmic(true)
                            .smallest_positive(1e-2)
                            .text(text)
                            .max_decimals(3)
                            .ui(ui);

                        if response.clicked() && temp_score == 0.0 {
                            if score.is_none() {
                                *score = Some(0.0);
                            } else {
                                *score = None;
                            }
                        } else if score.is_some() || temp_score != 0.0 {
                            *score = Some(temp_score / 100.0);
                        }
                    });
                };

            draw_slider(
                &mut self.min_score,
                "min score",
                0.0..=self.max_score.unwrap_or(0.0) * 100.0,
            );
            draw_slider(
                &mut self.max_score,
                "max score",
                self.min_score.unwrap_or(0.0) * 100.0..=100.0,
            );
        });
    }

    /// Draws the overview graph.
    fn draw_graph(&mut self, ui: &mut egui::Ui, rect: egui::Rect, scores: &[Option<NotNan<f64>>]) {
        let painter = ui.painter_at(rect);

        for (x, score) in score_pixel_iter(rect.width(), scores, self.min_score, self.max_score) {
            let color = super::utils::score_color(score);
            let max_score = scores
                .last()
                .copied()
                .flatten()
                .map(f64::from)
                .unwrap_or(1.0) as f32;
            let height = match score {
                Some(score) => score as f32 * rect.height() / max_score,
                None => rect.height(),
            }
            .clamp(1.0, f32::INFINITY);

            let column_rect = egui::Rect::from_min_size(
                rect.left_bottom() + egui::vec2(x, -height),
                egui::vec2(1.0, height),
            );

            painter.rect_filled(column_rect, 0.0, color);
        }
    }

    /// Determines if the given file should be included.
    pub(crate) fn should_include_file(&self, match_score: Option<f64>) -> bool {
        self.min_score <= match_score && match_score <= self.max_score
    }
}

/// Calculates the scores for each pixel along width pixels.
fn score_pixel_iter(
    width: f32,
    scores: &[Option<NotNan<f64>>],
    min: Option<f64>,
    max: Option<f64>,
) -> impl Iterator<Item = (f32, Option<f64>)> + '_ {
    let default_score = match (min, max) {
        (None, None) => None,
        (None, Some(max)) => Some(max / 2.0),
        (Some(_), None) => unreachable!(),
        (Some(min), Some(max)) => Some(max - min / 2.0),
    };
    let width = width.ceil() as u32;

    (0..width).map(move |pixel| {
        let pos = pixel as f64 / width as f64;
        let index = (pos * (scores.len() - 1) as f64).floor() as usize;

        let score = if index < scores.len() {
            scores[index].map(f64::from)
        } else {
            default_score
        };

        (pixel as f32, score)
    })
}

impl std::hash::Hash for FileFilter {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.min_score.map(f64::to_bits).hash(state);
        self.max_score.map(f64::to_bits).hash(state);
    }
}
