'use strict';

/* Services */

var exowebServices = angular.module('exowebServices', ['ngResource']);

exowebServices.factory('Device', ['$resource',
  function($resource){
    return $resource('devices/:deviceId.json', {}, {
      query: {method:'GET', params:{deviceId:'devices'}, isArray:true}
    });
  }]);

exowebServices.factory('User', ['$resource',
  function($resource){
    return $resource('users/:userId.json', {}, {
      query: {method:'GET', params:{userId:'users'}, isArray:true}
    });
  }]);
