#!/usr/bin/env bash
elm make src/ScheduleFromVideos.elm --output public/schedule-from-videos.js --optimize
elm make src/Config.elm --output public/config/config.js --optimize
cd public
# don't ask me why we have to explicitly include config.js here
/c/Program\ Files/7-Zip/7z a -r '-xr0!index.html' '-i!config/config.js' -bb3 ../releases/sfc.zip *.*
