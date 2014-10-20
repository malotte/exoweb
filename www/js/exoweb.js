'use strict';

/* App Module */

var exowebApp = angular.module('exoweb', [
    'ngRoute',
    'exowebControllers',
    'exowebDirectives',
    'exowebServices',
    'exowebFilters'
]);

exowebApp.config(['$routeProvider', '$locationProvider',
		  function($routeProvider, $locationProvider) {
		  
    $routeProvider.
      when('/signup', {
          templateUrl: 'html/signup.html'
      }).
      when('/confirm?account&email&session', {
          templateUrl: 'html/confirm.html'
      }).
      when('/confirm', {
          templateUrl: 'html/confirm.html'
      }).
      when('/login?name', {
          templateUrl: 'html/login.html'
      }).
      when('/login', {
          templateUrl: 'html/login.html'
      }).
      when('/device', {
          templateUrl: 'html/device.html'
      }).
      when('/user', {
          templateUrl: 'html/user.html'
      }).
      when('/howto', {
          templateUrl: 'html/howto.html'
      }).
      otherwise({
          redirectTo: '/device'
      });
 
      // $locationProvider.html5Mode(true);
  }]);
