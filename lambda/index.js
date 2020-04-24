'use strict';

let clientId = process.env['TWITCH_CLIENT_ID'];
let clientSecret = process.env['TWITCH_CLIENT_SECRET'];
const clientIdEncrypted = process.env['TWITCH_CLIENT_ID_ENCRYPTED'];
const clientSecretEncrypted = process.env['TWITCH_CLIENT_SECRET_ENCRYPTED'];
let AWS

if ((!clientId && clientIdEncrypted) || (!clientSecret && clientSecretEncrypted)) {
  AWS = require('aws-sdk');
}

var decrypt = function(encrypted, callback) {
  // Decrypt code should run once and variables stored outside of the
  // function handler so that these are decrypted once per container
  AWS.config.update({ region: 'us-east-1' });
  const kms = new AWS.KMS();
  try {
    const req = { CiphertextBlob: Buffer.from(encrypted, 'base64') };
    kms.decrypt(req, function(err, data) {
      if (err) return callback(err)
      var decrypted = data.Plaintext.toString('ascii');
      callback(null, decrypted)
    })
  } catch (err) {
    console.log('Decrypt error:', err);
    callback(err)
  }
};

const https = require('https')

var currentAccessTokenResponse = null
//var currentAccessTokenResponse = {access_token: 'xxxx'}

const tokenHostname = "id.twitch.tv"

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
  const tokenPath = "/oauth2/token"
    + "?client_id=" + clientId
    + "&client_secret=" + clientSecret
    + "&grant_type=client_credentials"
  const req = https.request({
    hostname: tokenHostname,
    path: tokenPath,
    method: 'POST',
    headers: standardHeaders,
    timeout: 5000,
  }, function(res) {
    //console.log('token response', res.statusCode);

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

var ensureToken = function(finalCallback, tryCallback) {
  var lastTry = function() {
    fetchToken(function(err, token) {
      if (err) {
        console.error(err)
        return finalCallback(err)
      }

      currentAccessTokenResponse = token

      tryCallback(null, currentAccessTokenResponse.access_token, finalCallback)
    })
  }

  if (currentAccessTokenResponse) {
    tryCallback(null, currentAccessTokenResponse.access_token, function(err, result) {
      if (err && err.status == 401) {
        lastTry()
      }

      finalCallback(err, result)
    })
  } else {
    lastTry()
  }
}

var fetchVideos = function(auth, userId, callback) {
  const req = https.request({
    hostname: apiHostname,
    path: videosPath + userId,
    method: 'GET',
    headers: authHeaders(auth),
    timeout: 5000,
  }, function(res) {
    //console.log('videos response', res.statusCode);

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

var receiveVideos = function(err, videos, callback) {
  if (err) {
    console.error(err)
    return callback(err)
  }

  //console.log(videos)
  var events = videos.data.filter(function(video) {
    return video.type == 'archive'
  }).map(function(video) {
    return {
      created_at: video.created_at,
      duration: video.duration,
    }
  })
  //console.log(events)
  callback(null, events)
}

var requestVideos = function(userId, callback) {
  ensureToken(callback, function(err, auth, done) {
    if (err) return callback(err)

    fetchVideos(auth, userId, function(error, videos) {
      receiveVideos(error, videos, function(err2, events) {
        if (err) return done(err)
        done(null, {
          events: events,
        })
      })
    });
  })
}

var withClientId = function(callback) {
  if (!clientId && clientIdEncrypted) {
    decrypt(clientIdEncrypted, function(err, result) {
      if (err) return callback(err)
      clientId = result
      callback()
    })
  } else {
    callback()
  }
}

var withClientSecret = function(callback) {
  if (!clientSecret && clientSecretEncrypted) {
    decrypt(clientSecretEncrypted, function(err, result) {
      if (err) return callback(err)
      clientSecret = result
      callback()
    })
  } else {
    callback()
  }
}

exports.handler = function(event, context, callback) {
  app.ports.lambdaEvent.send({
    kind: 'lambdaEvent',
    event: event,
  })
  /*
  withClientId(function(e1) {
    if (e1) return callback(e1)
    withClientSecret(function(e2) {
      if (e2) return callback(e2)
      requestVideos(event.user_id, callback)
    })
  })
  */
}

const elm = require('./handler')

const app = elm.Elm.Handler.init({flags: process.env});

var command = function(message) {
  //console.log(message)
  switch (message.kind) {
    default:
      console.log('unknown message', message)
      break;
  }
}

if (app.ports.lambdaCommand) {
  app.ports.lambdaCommand.subscribe(command)
}
