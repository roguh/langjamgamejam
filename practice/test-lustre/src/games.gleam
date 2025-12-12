import gleam/dict
import gleam/option.{None, Some}

const games = [
  #("@TicTacToe", "[[1,2,3],[4,5,6],[7,8,9]]"),
  #("@Chess", "[R, N, B, Q, K, B, N, R]"),
  #("@Pong", "______\n\n\n______"),
  #("@CardsForHumanity", "[card1, card2, judge]"),
]

pub fn names() -> List(String) {
  dict.keys(dict.from_list(games))
}

/// Get a game or try and load raw source code directly
pub fn load(game_str: String) -> String {
  case dict.get(dict.from_list(games), game_str) {
    Ok(game) -> game
    _ -> game_str
  }
}

/// Get a game or try and load raw source code directly
pub fn name(game_str: String) -> option.Option(String) {
  case dict.get(dict.from_list(games), game_str) {
    Ok(game) -> Some(game)
    _ -> None
  }
}
