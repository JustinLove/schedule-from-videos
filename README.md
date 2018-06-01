# Schedule From Videos

Displays when a Twich channel is likely to be live based on past broadcasts (videos).

## URL Parameters

- login : Twitch username of the channel to check hosts for. (Exactly one of login or userId is required.)
- userId : Twitch user id of the channel to check hosts for.

## Compiling

Built using [Elm](http://elm-lang.org/)

A [Twitch Client-ID](https://dev.twitch.tv/docs/authentication#registration) is required to make API calls. This is defined in `src/TwtichId.elm`. This file is not part of of the repo, but `src/TwitchId.elm.example` can be copied and edited to provide your client id.

My build command:

> `elm-make src/ScheduleFromVideos.elm --output public/schedule-from-videos.js`

`bin/monitor.bat` (and .sh) has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish.

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))

