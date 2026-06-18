# Clojure Crawler

A small text-based dungeon crawler written in Clojure. It is a learning project inspired by [Land of Lisp](https://www.amazon.com/Land-Lisp-Learn-Program-Game/dp/1593272812) and designed to run entirely in the terminal.

## What the game is

You start in a tiny world made up of a living room, garden, and attic. From there, you can explore rooms, read the descriptions, move between locations, pick up items, and check what you are carrying.

It is intentionally simple and does not have a win condition yet. The focus is on learning Clojure and experimenting with a classic text-adventure style game loop.

## How to run it

You need Java installed to run the release jar.

From the project root, launch the packaged game with:

```powershell
java -jar .\clojure-crawler-0.1.0-standalone.jar
```

If you want to run it from source during development, you can also use:

```powershell
clj -M:run
```

To rebuild the standalone jar:

```powershell
clj -T:build uber
```

## Commands

Type commands at the `>` prompt.

- `look` - Describe your current location, exits, and nearby items.
- `walk <direction>` - Move to another room, such as `walk west` or `walk upstairs`.
- `pickup <object>` - Pick up an object in the room, such as `pickup whiskey`.
- `inventory` - Show what you are carrying.
- `exit` - Quit the game.

## Examples

```text
> look
You are in the living-room. A wizard is snoring loudly on the couch.
There is a door going west from here.
There is a ladder going upstairs from here.
You see a whiskey on the floor.
You see a bucket on the floor.

> pickup whiskey
You are now carrying the whiskey.

> inventory
You are carrying: whiskey

> walk west
You are in a beautiful garden. There is a well in front of you.
There is a door going east from here.
You see a frog on the floor.
You see a chain on the floor.

> exit
Goodbye!
```

## Notes

- The standalone jar already includes the compiled game, so you do not need the `classes` folder to run it.
- Release builds should attach the jar from `target/` to GitHub Releases instead of committing generated files to the repository.
