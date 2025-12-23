import editor
import lang/yarn/cli

pub fn main() {
  case cli.main() {
    True -> editor.main()
    False -> Nil
  }
}
