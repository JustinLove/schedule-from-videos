- working plan: aws lambda, node
  - x extension rig
  - x cors
  - update error message - not necessarily twitch error
  - cdn
  - turn down logging after approval
- https://obsproject.com/tools/browser-drag-and-drop
- move day labels depending on data location?
- note about display timezone
- Twitch recommends user friendly error handling
- twitch specific ??? check jwt - twitch onAuthorized token
  - https://paste.fuelrats.com/acapajejob.js

[10:54:27] <wtfblub> not a big issue but the extension "Dark Reader" breaks the loading of your schedule from videos


- window size not received on first load in iframe (twitch dev rig)
- exceptions (from elm?) on first load in iframe

- window size lag

- wiggly updates
- color coding game/topic? - data not available
- timezones?

https://wondible.com/schedule-from-videos/

aws lambda add-permission   --function-name "arn:aws:lambda:us-east-1:731826986597:function:sfv_user_videos:ALIAS_NAME_HERE"   --source-arn "arn:aws:execute-api:us-east-1:731826986597:nwsj6y4eah/*/GET/user/*"   --principal apigateway.amazonaws.com   --statement-id 6bde4914-234a-46b0-889e-8ce508bbda4c   --action lambda:InvokeFunction

