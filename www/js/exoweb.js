'use strict';

/* App Module */

var exowebApp = angular.module('exoweb', [
    'ngRoute',
    'exowebControllers',
    'exowebServices',
    'exowebFilters'
]);

exowebApp.config(['$routeProvider', '$locationProvider',
		  function($routeProvider, $locationProvider) {
    var root = "~malotte/exoweb_js/";
    $routeProvider.
      when('/signup', {
          templateUrl: 'signup.html',
          controller: 'ApplyCtrl'
      }).
      when('/confirm?account&email&session', {
          templateUrl: 'confirm.html',
          controller: 'ConfirmCtrl'
      }).
      when('/login?name', {
          templateUrl: 'login.html',
          controller: 'LoginCtrl'
      }).
      when('/login', {
          templateUrl: 'login.html',
          controller: 'LoginCtrl'
      }).
     when('/devices/:deviceId', {
         templateUrl: 'device.html',
         controller: 'DeviceDetailCtrl'
      }).
      when('/devices', {
          templateUrl: 'devices.html',
          controller: 'DeviceListCtrl'
      }).
      when('/users', {
          templateUrl: 'users.html',
          controller: 'UserListCtrl'
      }).
      otherwise({
          redirectTo: '/'
      });
 
      // $locationProvider.html5Mode(true);
  }]);
