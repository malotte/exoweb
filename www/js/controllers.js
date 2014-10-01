'use strict';

/* Controllers */

var exowebControllers = 
    angular.module('exowebControllers', ['ngRoute']);


exowebControllers.controller('ApplyCtrl', ['$scope',
  function($scope) {
      
      $scope.apply = function(user) {
	  window.console.debug("User = " +user);
	  Wse.call('exoweb_apply', 'event', 
		   [Ei.atom('send'),
		    [Ei.tuple(Ei.atom('email'), user.email),
		     Ei.tuple(Ei.atom('password'), user.password)]], 
		   // reply callback
		   function(obj,ref,reply) {  
		       window.console.debug("Value = " +reply);
		       if (reply == "{ok, ok}") {
			   window.alert("Confirm mail sent to " +user.email);}
		       else if (reply.value[0] == "error") {
			   // Call not performed => {error, Reason}
			   window.alert("Error: " +reply.value[1]);
		       }
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
		   function(obj,ref,reply) {
		       window.console.debug("Reply = " +reply);
		       window.console.debug("Value 0 = " +reply.value[0]);
		       window.console.debug("Value 1 = " +reply.value[1]);
		       
		       if (reply.value[0] == "ok") {
			   // Call performed
			   var result = reply.value[1];
			   window.console.debug("Result = " +result.value[1]);
			   if (result.value[0] == "ok") {
			       // call sucessful => {ok,{ok,LoginName}}
			       window.location = 
				   "#/login?name="+result.value[1];
			   }
			   else if (result.value[0] == "error") {
			       // Call failed => {ok,{error,Reason}}
			       window.alert("Error: " +result.value[1]);
			   }
		       }
		       else if (reply.value[0] == "error") {
			   // Call not performed => {error, Reason}
			   window.alert("Error: " +reply.value[1]);
		       }
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
		   function(obj,ref,reply) {  
		       window.console.debug("Value = " +reply);
		       parseReply(reply, user);
		   });
      }

      var parseReply = function(reply, user) {
	  if (reply.value[0] == "ok") {
	      // Call performed
	      var result = reply.value[1];
	      window.console.debug("Result = " +result);
	      if (result == "ok") {
		  // call sucessful => {ok, ok}
		  createCookie(user);
	      }
	      else if (result.value[0] == "error") {
		  // Call failed => {ok,{error,Reason}}
		  window.alert("Error: " +result.value[1]);
	      }
	  }
	  else if (reply.value[0] == "error") {
	      // Call not performed => {error, Reason}
	      window.alert("Error: " +reply.value[1]);
	  }
      };

      var createCookie = function(user) {
	  Wse.start('exoweb_js', 'create_cookie', 
		    [[Ei.tuple(Ei.atom('name'), user.name),
		      Ei.tuple(Ei.atom('password'), user.password),
		      Ei.tuple(Ei.atom('path'), "#/index")]]);
      };

  }]);

exowebControllers.controller('LogoutCtrl', ['$scope', '$routeParams',
  function($scope) {
      
      // If not logged in redirect
      //$scope.$on('$routeChangeSuccess', function () {
      var redirect = function() {
	  var cookie = getCookie("id");
	  window.console.debug("Cookie = " +cookie);
	  if (cookie == "") {
	      window.console.debug("Empty cookie, redirecting!");
	      window.location.href = "#/login";
	  } 
      };

      $scope.logout = function() {
	  var cookie = document.cookie;
 	  window.console.debug("Cookie before reset " +cookie);
	  document.cookie="id=; expires=Thu, 01 Jan 1970 00:00:00 UTC";
	  /* document.cookie="id=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=//";
	     If adding path, add in create_cookie (exoweb_js.erl) as well */
	  cookie = document.cookie;
 	  window.console.debug("Cookie after reset " +cookie);
	  window.location.href = "#/login";
      };


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

      // Call this at load
      redirect();

  }]);


exowebControllers.controller('DeviceListCtrl', ['$scope', '$http',
    function($scope, $http) {
	$http.get('devices/devices.json').success(function(data) {
	    $scope.devices = data;
	});
	$scope.orderProp = 'device-id';
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

