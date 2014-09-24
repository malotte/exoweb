'use strict';

/* Controllers */

var applyMod = angular.module('applyMod', ['ngCookies']);


applyMod.controller('ApplyCtrl', ['$scope',
  function($scope) {
      
      $scope.apply = function(user) {
	  window.console.debug("User = " +user);
	  Wse.call('exoweb_apply', 'event', 
		   [Ei.atom('send'),
		    [Ei.tuple(Ei.atom('email'), user.email),
		     Ei.tuple(Ei.atom('password'), user.password)]], 
		   // reply callback
		   function(obj,ref,value) {  
		       window.console.debug("Value = " +value);
		       if (value == "{ok, ok}") {
			   window.alert("Confirm mail sent to " +user.email);}
		       else {
			   window.alert("Error: " +value);}
		   
		   });
      };

      $scope.passwordConfirmed = function(user) {
	  if (user.password != user.confirmpassword) {
	      window.alert("Passwords do not match! ");}
	  return angular.equals(user.password, user.confirmpassword);
      };

  }]);
