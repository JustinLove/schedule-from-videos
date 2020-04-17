'use strict';

var lambda = require('./index')

var main = function() {
  /*
  requestVideos('56623426', function(err, response) {
    if (err) {
      console.error(err)
      return
    }

    console.log(response)
  })
  */
  lambda.handler({user_id: '56623426'}, {}, function(err, response) {
    if (err) {
      console.error(err)
      return
    }

    console.log(response)
  })
}

main()
