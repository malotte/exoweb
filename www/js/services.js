'use strict';

/* Services */

var exowebServices = angular.module('exowebServices', []);

exowebServices.factory('ExowebUser', [ 'ExowebError',
    function(ExowebError) {

	var apply = function (user, okCallback, nokCallback) {
	  Wse.call('exoweb_apply', 'event', 
		   [Ei.tuple(Ei.atom('send'),
		    [Ei.tuple(Ei.atom('email'), user.email),
		     Ei.tuple(Ei.atom('password'), user.password)])], 
		   // reply callback
		   function(obj,ref,reply) {  
		       window.console.debug("Value = " +reply);
		       if (reply == "{ok, ok}") {
			   okCallback(user);
		       }
		       else if (reply.value[0] == "error") {
			   // Call not performed => {error, Reason}
			   nokCallback(reply.value[1]);
		       }})};

	var confirm = function(user, okCallback, nokCallback) {
	  Wse.call('exoweb_confirm', 'event', 
		   [Ei.tuple(Ei.atom('confirm'),
		    [Ei.tuple(Ei.atom('account'), user.account),
		     Ei.tuple(Ei.atom('email'), user.email),
		     Ei.tuple(Ei.atom('session'), user.session)])], 
		   // reply callback
		   function(obj,ref,reply) {
		       window.console.debug("Reply = " +reply);
		       parseUserReply(reply, okCallback, nokCallback);
		   })};

	var login = function(user, okCallback, nokCallback) {
	  Wse.call('exoweb_login', 'event', 
		   [Ei.tuple(Ei.atom('login'),
		    [Ei.tuple(Ei.atom('name'), user.name),
		     Ei.tuple(Ei.atom('password'), user.password)])], 
		   // reply callback
		   function(obj,ref,reply) {  
		       window.console.debug("Value = " +reply);
		       parseLoginReply(reply, user, okCallback, nokCallback);
		   })};

	var parseLoginReply = function(reply, user, okCallback, nokCallback) {
	    if (reply.value[0] == "ok") {
		// Call performed
		var result = reply.value[1];
		window.console.debug("Result = " +result);
		if (result == "ok") {
		    // call successful => {ok, ok}
		    okCallback(user);
		}
		else if (result.value[0] == "error") {
		    // Call failed => {ok,{error,Reason}}
		    nokCallback(result.value[1]);
		}
	    }
	    else if (reply.value[0] == "error") {
		// Call not performed => {error, Reason}
		nokCallback(reply.value[1]);
	    }};

      var createCookie = function(user) {
	  Wse.start('exoweb_js', 'create_cookie', 
		    [[Ei.tuple(Ei.atom('name'), user.name),
		      Ei.tuple(Ei.atom('password'), user.password),
		      Ei.tuple(Ei.atom('path'), "#/device")]])};
				  



	var parseUserReply = function(reply, okCallback, nokCallback) {
	    if (reply.value[0] == "ok") {		       
		if (reply.value[0] == "ok") {
		    // Call performed
		    var result = reply.value[1];
		    window.console.debug("Result = " +result.value[1]);
		    if (result.value[0] == "ok") {
			// call successful => {ok,{ok,User}}
			window.console.debug("User = " + result.value[1]);
			okCallback(result.value[1]);
		    }
		    else if (result.value[0] == "error") {
			// Call failed => {ok,{error,Reason}}
			ExowebError(result.value[1]);
		    }
		}
		else if (reply.value[0] == "error") {
		    // Call not performed => {error, Reason}
		    nokCallback(reply.value[1]);
		}
	    }
	};
	
	var checkCookie = function(okCallback, nokCallback) {
	    Wse.call('exoweb_js', 'wrapper', 
		     [Ei.atom('exoweb_login'),  // Module
		      Ei.atom('event'),          // Function
		      Ei.tuple(Ei.atom('user'),[])], // Args
		     // reply callback
		     function(obj,ref,reply) {  
			 window.console.debug("Reply = " +reply);
			 parseUserReply(reply, okCallback, nokCallback);
		     })
	};

	var deleteCookie = function() {
	    Wse.call('exoweb_js', 'delete_cookie',  [], 
		     function(obj,ref,reply) {  
			 window.console.debug("Value = " +reply);
		     })};

	return {
	    apply: apply,
	    confirm: confirm,
	    login: login,
	    createCookie: createCookie,
	    checkCookie: checkCookie,
	    deleteCookie: deleteCookie
	};
    }]);

exowebServices.factory('ExowebError', [
    function() {
	return function(error) {
		if (error == "illegal_cookie") {
		    window.alert("Session expired.");
		    window.console.debug("Go to login");
		    window.location.href = "#/login";
		}
		else {
		    window.alert("Error: " +error);
		}
	    }
	}]);
