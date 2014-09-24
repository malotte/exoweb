'use strict';

/* Controllers */

var loginMod = angular.module('loginMod', ['ngCookies']);



loginMod.controller('LoginCtrl', ['$scope', '$routeParams',
  function($scope) {
      
      $scope.login = function(user) {
	  window.console.debug("User = " +user);
	  Wse.call('exoweb_login', 'event', 
		   [Ei.atom('login'),
		    [Ei.tuple(Ei.atom('name'), user.name),
		     Ei.tuple(Ei.atom('password'), user.password)]], 
		   // reply callback
		   function(obj,ref,value) {  
		       window.console.debug("Value = " +value);
		   });
	  // If successful
	  Wse.start('exoweb_js', 'create_cookie', 
		    [[Ei.tuple(Ei.atom('name'), user.name),
		      Ei.tuple(Ei.atom('password'), user.password)]],
		    // reply callback
		    function(obj,ref,value) {  
		       window.console.debug("Value = " +value);
		    });
	  var cookie = $cookies.id;
	  window.console.debug("Cookie = " +cookie);
	  // window.location.href ="index.html";
      };

  }]);
loginMod.controller('LogoutCtrl', ['$scope', '$routeParams',
  function($scope) {
      
      $scope.logout = function() {
	  var cookie = $cookie.id;
 	  window.console.debug("Cookie reset " +cookie);
	  $cookie.id="";
	  location.href = "login.html";
      };

  }]);

