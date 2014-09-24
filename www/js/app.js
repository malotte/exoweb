'use strict';

/* App Module */

var exowebApp = angular.module('exoweb', [
    'ngRoute',
    'ngCookies',
    'exowebControllers',
    'exowebFilters',
    'exowebServices'
]);

exowebApp.config(['$routeProvider', '$locationProvider',
		  function($routeProvider, $locationProvider) {
    $routeProvider.
      when('/apply', {
          templateUrl: 'html/apply.html',
          controller: 'ApplyCtrl'
      }).
      when('/confirm?account&email&session', {
          templateUrl: 'html/confirm.html',
          controller: 'ConfirmCtrl'
      }).
      when('/login?name', {
          templateUrl: 'html/login.html',
          controller: 'LoginCtrl'
      }).
      when('/login', {
          templateUrl: 'html/login.html',
          controller: 'LoginCtrl'
      }).
     when('/devices/:deviceId', {
         templateUrl: 'html/device.html',
         controller: 'DeviceDetailCtrl'
      }).
      when('/devices', {
          templateUrl: 'html/devices.html',
          controller: 'DeviceListCtrl'
      }).
      when('/users', {
          templateUrl: 'html/users.html',
          controller: 'UserListCtrl'
      }).
      otherwise({
          redirectTo: '/'
      });
 
      // $locationProvider.html5Mode(true);
  }]);
