var app = Elm.ScheduleFromVideos.fullscreen()

if (window.Twitch && window.Twitch.ext) {
  window.Twitch.ext.onAuthorized(function(auth) {
    console.log('auth', auth);
  });

  window.Twitch.ext.onContext(function(context, contextFields) {
    console.log('context', context);
    console.log('fields', contextFields);
  });
  
  window.Twitch.ext.onError(function(err) {
    console.error(err);
  });
}
