'use strict';

/* Controllers */

var exowebControllers = 
    angular.module('exowebControllers', ['ngRoute']);


exowebControllers.controller('ApplyCtrl', [
    '$scope', 'ExowebUser',
    function($scope, ExowebUser) {
      var applyOkCallback = function(user) {
	  window.alert("Confirm mail sent to " + user.email);
      }
	
      var applyNokCallback = function(error) {
	  window.alert("Error: " + error);
      }
 
      $scope.apply = function(user) {
	  window.console.debug("User = " +user);
	  ExowebUser.apply(user, applyOkCallback, applyNokCallback);
      }

      $scope.passwordConfirmed = function(user) {
	  if (user.password != user.confirmpassword) {
	      window.alert("Passwords do not match! ");}
	  return angular.equals(user.password, user.confirmpassword)};
	
  }]);


exowebControllers.controller('ConfirmCtrl', [
    '$scope', '$routeParams', 'ExowebUser',
    function($scope, $routeParams, ExowebUser) {
      $scope.account = $routeParams.account;
      $scope.email = $routeParams.email;
      $scope.session = $routeParams.session;
      
      var okCallback = function(username) {
	  window.location =  "#/login?name="+ username;
      }
    
      var nokCallback = function(error) {
	  window.alert("Error: " + error);
      }
 
      $scope.confirm = function() {
	 var user = new Object;
	 user.email = $scope.email;
	 user.account = $scope.account;
	 user.session = $scope.session;
	 ExowebUser.confirm(user, okCallback, nokCallback);
     }
 }]);


exowebControllers.controller('LoginCtrl', ['$scope', 'ExowebUser',
  function($scope, ExowebUser) {
      
      var okCallback = function(user) {
	  ExowebUser.createCookie(user)
      }
    
      var nokCallback = function(error) {
	   window.alert("Error: " + error);
      }

      $scope.login = function(user) {
	  window.console.debug("User = " +user);
 	  ExowebUser.login(user, okCallback, nokCallback);
     }


  }]);

exowebControllers.controller('MenuCtrl', [
    '$scope', '$routeParams', 'ExowebUser',
    function($scope, $routeParams, ExowebUser) {
      
	var okCallback = function(user) {
	    $scope.user = user;
	}
	 
	 var nokCallback = function(error) {
	     window.alert("Error: " + error);
	 }

 	// Remove when made ngCookies work
	var getCookie = function(cname) {
	    var name = cname + "=";
	    var ca = document.cookie.split(';');
	    window.console.debug("getCookie => " +ca);
	    for(var i=0; i<ca.length; i++) {
		var c = ca[i];
		while (c.charAt(0)==' ') c = c.substring(1);
		if (c.indexOf(name) != -1) 
		    return c.substring(name.length, c.length);
	    }
	    return "";
	};
	 
	// If not logged in redirect
	var redirect = function() {
	    var cookie = getCookie("id");
	    window.console.debug("Cookie = " +cookie);
	    if (cookie == "") {
		window.console.debug("Empty cookie, redirecting!");
		window.location.href = "#/login";
	    }
	    else {
		ExowebUser.checkCookie(okCallback, nokCallback);
	    }
	};
	
	// Called when user clicks logout-button
	$scope.logout = function() {
	     var cookie = document.cookie;
 	     window.console.debug("Cookie before reset " + cookie);
	     document.cookie="id=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/";
	     cookie = document.cookie;
 	     window.console.debug("Cookie after reset " + cookie);
	     ExowebUser.deleteCookie(); 
	     window.location.href = "#/login";
	 };

	redirect();

     }]);
