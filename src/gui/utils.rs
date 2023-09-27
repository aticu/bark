//! Common GUI utility functions used elsewhere.

use eframe::egui::{self, Color32};

/// A table of colors that are supposed to be useful for categorical data differentiation.
///
/// Slightly modified from
/// [https://sashamaps.net/docs/resources/20-colors/](https://sashamaps.net/docs/resources/20-colors/).
const COLOR_TABLE: &[Color32] = &[
    egui::Color32::from_rgb(0, 130, 200),
    egui::Color32::from_rgb(128, 0, 0),
    egui::Color32::from_rgb(145, 30, 180),
    egui::Color32::from_rgb(70, 240, 240),
    egui::Color32::from_rgb(240, 50, 230),
    egui::Color32::from_rgb(210, 245, 60),
    egui::Color32::from_rgb(250, 190, 212),
    egui::Color32::from_rgb(0, 128, 128),
    egui::Color32::from_rgb(220, 190, 255),
    egui::Color32::from_rgb(245, 130, 48),
    egui::Color32::from_rgb(170, 110, 40),
    egui::Color32::from_rgb(255, 250, 200),
    egui::Color32::from_rgb(170, 255, 195),
    egui::Color32::from_rgb(128, 128, 0),
    egui::Color32::from_rgb(255, 215, 180),
];

/// Returns a color to represent categorical data for the given index.
pub(crate) fn categorical_color(index: usize) -> Color32 {
    COLOR_TABLE[index % COLOR_TABLE.len()]
}

/// Linearly interpolates between the two given colors.
///
/// If `between` is `0.0`, the result will be `color1` and if it is `1.0`, the result will be
/// `color2`.
pub(crate) fn lerp_color(color1: Color32, color2: Color32, mut between: f64) -> Color32 {
    between = if between.is_nan() {
        0.5
    } else {
        between.clamp(0.0, 1.0)
    };

    let transform = |val1, val2| (val1 as f64 * (1.0 - between) + val2 as f64 * between) as u8;

    Color32::from_rgba_unmultiplied(
        transform(color1.r(), color2.r()),
        transform(color1.g(), color2.g()),
        transform(color1.b(), color2.b()),
        transform(color1.a(), color2.a()),
    )
}

/// Returns the color for a match score.
pub(crate) fn score_color(score: Option<f64>) -> Color32 {
    match score {
        Some(score) => {
            // re-scale the score color such that the middle point (0.5) between the colors
            // is at 0.05
            // this is computed by 0.5_f64.ln() / 0.05_f64.ln()
            const POWER: f64 = 0.23137821315975918;
            lerp_color(Color32::RED, Color32::GREEN, score.powf(POWER))
        }
        None => Color32::DARK_GRAY,
    }
}

/*
/// A button where the contents are determined dynamically.
pub(crate) fn ui_button(
    ui: &mut egui::Ui,
    add_contents: impl FnOnce(&mut egui::Ui),
) -> egui::Response {
    let response = sub_ui(ui, egui::Sense::click(), 5.0, |ui| {
        ui.with_layer_id(
            // note that this layer id is a bit of a hack, but it allows to draw the rectangle
            // behind the contents
            egui::LayerId {
                order: egui::Order::PanelResizeLine,
                id: ui.next_auto_id(),
            },
            add_contents,
        );
    });
    ui.painter().rect(
        response.rect,
        ui.style().interact(&response).rounding,
        ui.style().interact(&response).bg_fill,
        ui.style().interact(&response).bg_stroke,
    );
    response
}
*/

/// Adds the contents into an inner `Ui` with a given margin and sensing.
pub(crate) fn sub_ui(
    ui: &mut egui::Ui,
    sense: egui::Sense,
    margin: f32,
    add_contents: impl FnOnce(&mut egui::Ui),
) -> egui::Response {
    let min = ui.cursor().min;
    let margin_vec = egui::vec2(margin, margin);
    let size_vec = match ui.layout().main_dir() {
        egui::Direction::LeftToRight | egui::Direction::RightToLeft => {
            egui::vec2(f32::INFINITY, 0.0)
        }
        egui::Direction::TopDown | egui::Direction::BottomUp => egui::vec2(0.0, f32::INFINITY),
    };
    let inner_rect = ui
        .allocate_ui_at_rect(
            egui::Rect::from_min_size(min + margin_vec, size_vec),
            add_contents,
        )
        .response
        .rect;
    let rect = egui::Rect::from_min_size(min, inner_rect.size() + margin_vec + margin_vec);
    ui.allocate_rect(rect, sense)
}
