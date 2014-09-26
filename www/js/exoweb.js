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
		  
    window.console.debug("hej");
 		     
    $routeProvider.
     when('/menu', {
          templateUrl: 'html/menu.html',
          controller: 'LogoutCtrl'
      }).
     when('/signup', {
          templateUrl: 'html/signup.html',
          controller: 'ApplyCtrl'
      }).
      when('/confirm?account&email&session', {
          templateUrl: 'html/confirm.html',
          controller: 'ConfirmCtrl'
      }).
      when('/confirm', {
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
          redirectTo: '/menu'
      });
 
      // $locationProvider.html5Mode(true);
  }]);
