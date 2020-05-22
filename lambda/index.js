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

exports.handler = function(event, context, callback) {
  app.ports.lambdaEvent.send({
    kind: 'lambdaEvent',
    state: callback,
    event: event,
  })
}

const elm = require('./handler')

const app = elm.Elm.Handler.init({flags: process.env});

var decryptValues = function(encrypted, decrypted, state) {
  var encValue = encrypted.shift()
  decrypt(encValue, function(err, decValue) {
    if (err) {
      app.ports.lambdaEvent.send({
        kind: 'decryptionError',
        state: state,
        error: err.toString(),
      })
      return
    }
    decrypted.push(decValue)
    if (encrypted.length < 1) {
      app.ports.lambdaEvent.send({
        kind: 'decrypted',
        state: state,
        values: decrypted,
      })
    } else {
      decryptValues(encrypted, decrypted, state)
    }
  })
}

var httpRequest = function(info, state) {
  const req = https.request({
    hostname: info.hostname,
    path: info.path,
    method: info.method,
    headers: info.headers,
    timeout: 5000,
  }, function(res) {
    console.log('request response', info.id, res.statusCode);

    let rawData = '';
    res.on('data', (chunk) => { rawData += chunk; });
    res.on('end', () => {
      if (res.statusCode == 200) {
        app.ports.lambdaEvent.send({
          kind: 'httpResponse',
          state: state,
          id: info.id,
          body: rawData,
        })
      } else {
        console.log(rawData)
        app.ports.lambdaEvent.send({
          kind: 'badStatus',
          state: state,
          id: info.id,
          status: res.statusCode,
          body: rawData,
        })
      }
    });
  })

  req.on('error', function(err) {
    console.error('request failed', err);
    app.ports.lambdaEvent.send({
      kind: 'networkError',
      state: state,
      id: info.id,
      error: err,
    })
  })

  req.end()
}


var command = function(message) {
  //console.log(message)
  switch (message.kind) {
    case 'decrypt':
      decryptValues(message.values, [], message.state)
      break;
    case 'httpRequest':
      httpRequest(message.request, message.state)
      break;
    case 'success':
      message.session(null, message.data)
      break;
    case 'error':
      message.session(message.error)
      break;
    default:
      console.log('unknown message', message)
      break;
  }
}

if (app.ports.lambdaCommand) {
  app.ports.lambdaCommand.subscribe(command)
}
