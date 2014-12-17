//
// Copyright (C) 2007 - 2014, Rogvall Invest AB, <tony@rogvall.se>
//
// This software is licensed as described in the file COPYRIGHT, which
// you should have received as part of this distribution. The terms
// are also available at http://www.rogvall.se/docs/copyright.txt.
//
// You may opt to use, copy, modify, merge, publish, distribute and/or sell
// copies of the Software, and permit persons to whom the Software is
// furnished to do so, under the terms of the COPYRIGHT file.
//
// This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
// KIND, either express or implied.
//
//---- END COPYRIGHT ---------------------------------------------------------
//
// Exoweb controllers
//
// Author: Marina Westman Lönne
// Created: October 2014
//
//----------------------------------------------------------------------------

'use strict';


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
 
	$scope.apply = function(user, reCAPTCHA) {
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
	  window.console.debug("User = " +JSON.stringify(user));
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
