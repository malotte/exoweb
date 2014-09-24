'use strict';

/* Controllers */

var exowebControllers = 
    angular.module('exowebControllers', ['ngRoute','ngCookies']);


exowebControllers.controller('ApplyCtrl', ['$scope',
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


exowebControllers.controller('ConfirmCtrl', 
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
		       $window.location = "login.html";
		   });
      };
  }]);


exowebControllers.controller('LoginCtrl', ['$scope', '$routeParams',
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
	  var cookie = document.cookies;
	  window.console.debug("Cookie = " +cookie);
	  window.location.href ="index.html";
      };

  }]);
exowebControllers.controller('LogoutCtrl', ['$scope', '$routeParams',
  function($scope) {
      
      $scope.logout = function() {
	  var cookie = document.cookie;
 	  window.console.debug("Cookie before reset " +cookie);
	  document.cookie="id=; expires=Thu, 01 Jan 1970 00:00:00 UTC";
	  /* document.cookie="id=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/";
	     If adding path, add in create_cookie (exoweb_js.erl) as well */
	  cookie = document.cookie;
 	  window.console.debug("Cookie after reset " +cookie);
	  location.href = "login.html";
      };

  }]);


exowebControllers.controller('DeviceListCtrl', ['$scope', 'Device',
  function($scope, Device) {
    $scope.devices = Device.query();
    $scope.orderProp = 'age';
  }]);

exowebControllers.controller('DeviceDetailCtrl', ['$scope', '$routeParams', 'Device',
  function($scope, $routeParams, Device) {
    $scope.device = Device.get({deviceId: $routeParams.deviceId}, function(device) {
      $scope.mainImageUrl = device.images[0];
    });

    $scope.setImage = function(imageUrl) {
      $scope.mainImageUrl = imageUrl;
    }
  }]);

exowebControllers.controller('UserListCtrl', ['$scope', 'User',
  function($scope, User) {
    $scope.users = User.query();
    $scope.orderProp = 'name';
  }]);

