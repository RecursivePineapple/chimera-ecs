
use std::collections::HashMap;

use serde::{Serialize, Deserialize};


#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum CommandParameterType {
    Int,
    Float,
    String,
    Boolean
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CommandParameter {
    param_type: CommandParameterType,
    optional: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct CommandDefinition {
    command: String,
    params: HashMap<String, CommandParameter>,
}

#[derive(Serialize)]
#[allow(dead_code)]
pub enum CommandQuery {
    GetCommands{ }
}
