var app = Elm.ScheduleFromVideos.init()

var canvas
var context
var measureQueue = []

function tryToGetValidCanvas(time) {
  canvas = document.createElement("canvas");
  context = canvas.getContext("2d");
  try {
    context.font = "10px sans-serif";
    measureQueue.forEach(getTextWidth)
    measureQueue = []
  } catch (e) {
    //console.log(e)
    canvas = null
    context = null
    //console.log("canvas exception, trying again in", time)
    setTimeout(tryToGetValidCanvas, time, time*2)
  }
}

let getTextWidth = function getTextWidth(params) {
  if (context) {
    context.font = params.font;
    let metrics = context.measureText(params.text);
    app.ports.textSize.send({text: params.text, width: metrics.width})
  } else {
    measureQueue.push(params)
  }
}

app.ports.getTextWidth.subscribe(getTextWidth)

tryToGetValidCanvas(16)

if (window.Twitch && window.Twitch.ext) {
  window.Twitch.ext.onAuthorized(function onAuthorized(auth) {
    //console.log('auth', auth);
    auth.helixToken = auth.helixToken || '' // not available in rig, and reported missing on mobile
    app.ports.onAuthorized.send(auth)
  });

  window.Twitch.ext.onContext(function onContext(context, contextFields) {
    //console.log('context', context);
    //console.log('fields', contextFields);
    app.ports.onContext.send(context)
  });
  
  window.Twitch.ext.onError(function onError(err) {
    //console.error(err);
    app.ports.onError.send(err)
  });
}

if (app.ports.logCommand) {
  app.ports.logCommand.subscribe(function(message) {
    switch(message.kind) {
      case 'log':
        if (window.Twitch && window.Twitch.ext) {
          window.Twitch.ext.rig.log(message.note, message.value)
        } else {
          console.log(message.note, message.value)
        }
        break
      default:
        console.log('unknown message', message)
        break;
    }
  })
}
