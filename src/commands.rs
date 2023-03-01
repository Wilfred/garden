use strum_macros::EnumIter;

#[derive(Debug, EnumIter)]
pub enum Commands {
    Help,
    Globals,
    Locals,
    Parse(String),
    Source,
    Stack,
}

impl Commands {
    pub fn from_string(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            ":help" => Some(Commands::Help),
            ":globals" => Some(Commands::Globals),
            ":locals" => Some(Commands::Locals),
            ":source" => Some(Commands::Source),
            ":stack" => Some(Commands::Stack),
            _ => {
                if let Some(src) = s.strip_prefix(":parse ") {
                    Some(Commands::Parse(src.to_owned()))
                } else {
                    None
                }
            }
        }
    }

    pub fn to_string(&self) -> &str {
        match self {
            Commands::Help => ":help",
            Commands::Globals => ":globals",
            Commands::Locals => ":locals",
            Commands::Parse(_) => ":parse",
            Commands::Source => ":source",
            Commands::Stack => ":stack",
        }
    }
}
