#!/usr/bin/env bash
elm make src/ScheduleFromVideos.elm --output public/schedule-from-videos.js --optimize
elm make src/Config.elm --output public/config/config.js --optimize
