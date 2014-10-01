'use strict';

/* Services */

var exowebServices = angular.module('exowebServices', ['ngResource']);
/*
exowebServices.factory('Device', ['$resource',
  function($resource){
    return $resource('devices/:deviceId.json', {}, {
      query: {method:'GET', params:{deviceId:'devices'}, isArray:true}
    });
  }]);
*/
exowebServices.factory('User', ['$resource',
  function($resource){
    return $resource('users/:userId.json', {}, {
      query: {method:'GET', params:{userId:'users'}, isArray:true}
    });
  }]);

/*exowebServices.factory('Device', ['$resource',
  function($resource){
    return $resource('https://localhost:8000/exodm/rpc', {}, {
	query: {method:'POST', 
		data:
		'{"json-rpc": "2.0", "method": "exodm:list-devices", "id": "1","params": {"n": 10,"previous": ""}}', 
		headers:{"Authorization": "Basic " +btoa("m17/admin:111111") },
		isArray:true}
    });
  }]);
*/