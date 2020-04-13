'use strict';

const clientId = process.env['TWITCH_CLIENT_ID'];
const clientSecret = process.env['TWITCH_CLIENT_SECRET'];

const https = require('https')

var currentAccessTokenResponse = null

const tokenHostname = "id.twitch.tv"
const tokenPath = "/oauth2/token"
  + "?client_id=" + clientId
  + "&client_secret=" + clientSecret
  + "&grant_type=client_credentials"

const apiHostname = "api.twitch.tv"
const videosPath = "/helix/videos?first=100&type=archive&user_id="

const standardHeaders = {
  "User-Agent": "Schedule From Videos Lambda",
}

var authHeaders = function(token) {
  var headers = Object.create(standardHeaders)
  Object.assign(headers, {
    "Client-ID": clientId,
    "Authorization": ("Bearer " + token)
  })
  return headers
}


var fetchToken = function(callback) {
  const req = https.request({
    hostname: tokenHostname,
    path: tokenPath,
    method: 'POST',
    headers: standardHeaders,
    timeout: 5000,
  }, function(res) {
    console.log('token response', res.statusCode);

    let rawData = '';
    res.on('data', (chunk) => { rawData += chunk; });
    res.on('end', () => {
      try {
        const parsedData = JSON.parse(rawData);
        if (res.statusCode == 200) {
          callback(null, parsedData)
        } else {
          callback(parsedData)
        }
      } catch (e) {
        console.error(e.message);
        callback(e)
      }
    });
  })

  req.on('error', function(err) {
    console.error('token request failed', err);
    callback(err)
  })

  req.end()
}

var fetchVideos = function(auth, userId, callback) {
  const req = https.request({
    hostname: apiHostname,
    path: videosPath + userId,
    method: 'GET',
    headers: authHeaders(auth),
    timeout: 5000,
  }, function(res) {
    console.log('videos response', res.statusCode);

    let rawData = '';
    res.on('data', (chunk) => { rawData += chunk; });
    res.on('end', () => {
      try {
        const parsedData = JSON.parse(rawData);
        if (res.statusCode == 200) {
          callback(null, parsedData)
        } else {
          callback(parsedData)
        }
      } catch (e) {
        console.error(e.message);
        callback(e)
      }
    });
  })

  req.on('error', function(err) {
    console.error('token request failed', err);
    callback(err)
  })

  req.end()
}

var requestVideos = function(userId, callback) {
  fetchToken(function(err, token) {
    if (err) {
      console.error(err)
      return callback(err)
    }

    currentAccessTokenResponse = token

    fetchVideos(token.access_token, userId, function(error, videos) {
      if (error) {
        console.error(error)
        return callback(error)
      }

      console.log(videos)
      callback(null, videos)
    })
  })
}

exports.handler = function(event, context, callback) {
  callback(null, '');
}

var main = function() {
  requestVideos('56623426', function(err, response) {
    if (err) {
      console.error(err)
      return callback(err)
    }
  })
}

main()
