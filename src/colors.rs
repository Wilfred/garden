use owo_colors::{OwoColorize, Stream};

pub(crate) fn green(s: &str) -> String {
    s.if_supports_color(Stream::Stdout, |text| text.green())
        .to_string()
}

pub(crate) fn bold_green(s: &str) -> String {
    s.if_supports_color(Stream::Stdout, |text| text.green())
        .if_supports_color(Stream::Stdout, |text| text.bold())
        .to_string()
}
