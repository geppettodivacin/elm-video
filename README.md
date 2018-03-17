Example Elm Video Player
========================

I wanted to figure out how a video player in Elm would work, so I made one.

There were two primary challenges: sending commands to the video player (such
as play and pause) and reading the state of the video player (such as current
position). The first problem was solved through a port: JavaScript listens for
messages and acts accordingly upon the video object. The second was solved
through custom event handlers. Since each video event has a reference to the
media player object, the event target can be decoded to provide the necessary
information.

The assets and HTML template were taken from the 2013 article
https://www.creativebloq.com/html5/build-custom-html5-video-player-9134473,
which was a decent primer on how to make a custom video player. However, noting
its age, there may be better ways to do parts of this at the time of writing
that I am as yet unaware of.

The elm can be compiled using the following `elm-make` command:

    elm-make src/media-player.elm --output elm.js

You can then open media-player.html in your favorite web browser and see the
results. Note that the `--debug` flag can be added to add a debug box at the
bottom which tracks internal Elm messages.
