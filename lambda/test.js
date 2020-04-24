'use strict';

var lambda = require('./index')

var main = function() {
  lambda.handler({user_id: '56623426'}, {}, function(err, response) {
    if (err) {
      console.error(err)
      return
    }

    console.log(response)
  })
}

main()
