'use strict';

var lambda = require('./index')

var main = function() {
  lambda.handler({event: 'videoswithname', user_id: '56623426'}, {}, function(err, response) {
    if (err) {
      console.error(err)
      return
    }

    console.log(response)
  })
}

main()
