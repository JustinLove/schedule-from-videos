# Schedule From Videos

Displays when a Twitch channel is likely to be live based on past broadcasts (videos).

## URL Parameters

- login : Twitch username of the channel to check hosts for. (Exactly one of login or userId is required.)
- userId : Twitch user id of the channel to check hosts for.

## Development

Built using [Elm](http://elm-lang.org/)

### Frontend

A backend service (see below) is required to proxy twitch api calls. The backend service url is defined in `src/Backend.elm`. This file is not part of of the repo, but `src/Backend.elm.example` can be copied and edited to provide the url.

My build command:

> `elm-make src/ScheduleFromVideos.elm --output public/schedule-from-videos.js`

`bin/monitor.bat` (and .sh) has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish.

### Backend

Backend is currently defined as an [API Gateway](https://aws.amazon.com/api-gateway/) wrapping an [AWS Lambda Function](https://aws.amazon.com/lambda/). Backend files are in the `lambda` subdirectory.

`bin/lambda_monitor.sh` has a command using the [watch](https://www.npmjs.com/package/watch) CLI to compile `handler.js`

AWS Configuration requires further documentation. In brief.

A [Twitch Client-ID and Secret](https://dev.twitch.tv/docs/authentication#registration) are required to make API calls.

Lambda function consists of both `index.js`(static) and `handler.js` (compiled from elm). On AWS it requires the environment variables `TWITCH_CLIENT_ID_ENCRYPTED` and `TWITCH_CLIENT_SECRET_ENCRYPTED`, which are decrypted via Key Managment Service.

The gateway is configured with these urls:

```
GET /user/{user_name}
Lambda Event: { event: "user", user_name: ... }
GET /videos/{user_id}
Lambda Event: { event: "videos", user_id: ... }
GET /videoswithname/{user_id}
Lambda Event: { event: "videoswithname", user_id: ... }
```

#### Local Testing

Local function testing has been performed with

`foreman start lambda`

The lambda task peforms `node test.js`, with foreman provideing the env variables `TWITCH_CLIENT_ID` and `TWITCH_CLIENT_SECRET` from the `.env` file.

`test.js` makes a single function call; edit the file to tigger different events.

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))

