import gleam/dict
import gleam/option.{type Option, None, Some}

import lang/yarn/embedded.{assets}

pub fn names() -> List(String) {
  dict.keys(dict.from_list(assets))
}

/// Get a game or try and load raw source code directly
pub fn load(name: String) -> String {
  case dict.get(dict.from_list(assets), name) {
    Ok(game) -> game
    _ -> name
  }
}

/// Get a game or try and load raw source code directly
pub fn name(name: String) -> Option(String) {
  case dict.get(dict.from_list(assets), name) {
    Ok(_) -> Some(name)
    _ -> None
  }
}
