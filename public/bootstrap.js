var app = Elm.ScheduleFromVideos.init()

if (window.Twitch && window.Twitch.ext) {
  window.Twitch.ext.onAuthorized(function(auth) {
    //console.log('auth', auth);
    app.ports.onAuthorized.send(auth)
  });

  window.Twitch.ext.onContext(function(context, contextFields) {
    //console.log('context', context);
    //console.log('fields', contextFields);
    app.ports.onContext.send(context)
  });
  
  window.Twitch.ext.onError(function(err) {
    //console.error(err);
    app.ports.onError.send(err)
  });
}
