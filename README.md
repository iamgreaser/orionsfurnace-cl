# Orion's Furnace (Lisp version)
### GreaseMonkey & contributors

This is the Lisp version of Orion's Furnace, which is a rewrite of the original C++ version, which had networking working but felt like too much of a chore to do get it to the point where there was any meaningful gameplay.

To get this working, make sure you have QuickLisp installed in your Common Lisp implementation, which is probably going to be Steel Bank Common Lisp (SBCL), and if it isn't then if you're having a hard time running this game then you should probably use SBCL.

This should be sufficient to get it running nicely:

    ln -s "$(pwd)/src/orions-furnace" "~/quicklisp/local-projects/orions-furnace"

Although I'm probably doing it all wrong, so expect these instructions to change. --GM

Running the game once set up properly should be as easy as running this in a REPL:

    (ql:quickload #:orions-furnace)
    (orions-furnace:run)

But if you're running this on a Mac, you'll want to shove it into the main thread or something because cl-sdl2 apparently needs it when you're using SBCL on a Mac. If you ARE running this on a Mac, let us know so we can get the instructions sorted out here.

## License

AGPLv3 once I get around to putting it on properly. --GM
