#!/usr/bin/env python3
# coding: utf-8
import glob
import json
import os.path


def parse(fname: str) -> str:
    with open(fname) as f:
        contents = f.read()
    return json.dumps(contents.replace("/", "/"))


def generate(base_dir: str, extension: str, output: str):
    contents = (
        "pub const assets = ["
        + "\n  ".join(
            [
                f"#({json.dumps(n.replace(base_dir, ''))}, {parse(n)}),"
                for n in glob.glob(base_dir + "/*/*" + extension)
            ]
        )
        + "\n]"
    )
    if os.path.isfile(output):
        with open(output) as f:
            if f.read() == contents:
                print("skipping", output)
                return
    with open(output, "w") as f:
        f.write(contents)


def main():
    generate("./assets/dialog", ".yarn", "./src/lang/yarn/embedded.gleam")


if __name__ == "__main__":
    main()
