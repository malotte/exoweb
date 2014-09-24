'use strict';

/* Controllers */

var confirmMod = angular.module('confirmMod', ['ngRoute','ngCookies']);


confirmMod.controller('ConfirmCtrl', 
			     ['$scope', '$routeParams',
			      function($scope, $routeParams) {
      $scope.account = $routeParams.account;
      $scope.email = $routeParams.email;
      $scope.session = $routeParams.session;
      
      $scope.confirm = function() {
	  Wse.call('exoweb_confirm', 'event', 
		   [Ei.atom('confirm'),
		    [Ei.tuple(Ei.atom('account'), $scope.account),
		     Ei.tuple(Ei.atom('email'), $scope.email),
		     Ei.tuple(Ei.atom('session'), $scope.session)]], 
		   // reply callback
		   function(obj,ref,value) {
		       window.console.debug("Value = " +value);
		       // parse reply for login name
		       $window.location = '/login?name=m925//admin';
		   });
      };
  }]);

