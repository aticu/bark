//! Common GUI utility functions used elsewhere.

use eframe::egui::{self, Color32};

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
