import editor
import lang/yarn/cli

pub fn main() {
  case editor.main() {
    True -> cli.main()
    False -> Nil
  }
}
