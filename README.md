# 10 Doujins per Second

A game I made with some good friends for Ludum Dare 51.

Code's crap but it's in F# and surprisingly bug-free. All jank is completely intentional.

## A bit about the code

For the last game I just ended up with a much less functional approach, most of the game state was held in a few global optional variables, it was basically just as bad as your regular C# game and probably still has a few latent bugs. This time I went with a much more typed approach where the game state was mostly held into a `GameStateMachine` enum and the `Update` method is basically a giant match expression that takes a GameState and returns a new one, so it approaches The Elm Architecture a bit more.

I still haven't fully figured out how to factor all of this state into smaller, more understandable modules, and there's still quite a bunch of global state and shortcuts I took because of the time limits but I can see how they could fit into the `GameStateMachine`. I'd like to build a more structured framework around it but in the context of a game jam the mutable state hacks are much more pragmatic, so it'll probably be mostly helper functions and _maybe_ a wrapper around `Game`, but who knows.

## License

The code (all the text files) is unlicensed, grab it, copy it, go nuts.

The art is probably gonna be CC BY-NC-SA, I'll have to talk it out with all the authors first.
