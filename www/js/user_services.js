'use strict';

/* Services */

var exowebUserServices = angular.module('exowebUserServices', []);


exowebUserServices.factory('UserList', ['ExowebError',
    function(ExowebError) {

	var users = [];

	function fillTable(dataArray, callback) {
	    var i;
	    // Loop over users
	    for (i=0; i < dataArray.length; i++){
		var j, roleArray;
		var user = new Object;
		dataArray[i] = Wse.decode_js(dataArray[i]);
		window.console.debug("User " + i + " = " + dataArray[i]);
		roleArray =  dataArray[i].roles;
		user["name"] = (dataArray[i])["name"];
		// Get first role
		user["role"] = roleArray[0];
		users[i] = user;
		window.console.debug("User " + i + " = " + users[i]);
	    }
	    callback();
	};


	function parseListReply(reply, callback) {
	    if (reply.value[0] == "ok") {		       
	      if (reply.value[0] == "ok") {
		  // Call performed
		  var result;
		  result = reply.value[1];
		  window.console.debug("Result = " +result.value[1]);
		  if (result.value[0] == "ok") {
		      // call successful => {ok,{ok, Data}}
		      fillTable(result.value[1], callback);
		  }
		  else if (result.value[0] == "error") {
		      // Call failed => {ok,{error,Reason}}
		      ExowebError(result.value[1]);
		  }
	      }
	      else if (reply.value[0] == "error") {
		  // Call not performed => {error, Reason}
		  window.alert("Error: " +reply.value[1]);
	      }
	  }
	};

	var getData = 
	    function(pagingOptions, selectOptions, filterOptions, callback) {
		users.splice(0, users.length);
		window.console.debug("Last = " + selectOptions.lastId);
		window.console.debug("Last page = " + selectOptions.lastPage);
		setTimeout(function () {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_user'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('load'),  // Args
				   [Ei.tuple(Ei.atom('rows'), 
					     pagingOptions.pageSize),
				    Ei.tuple(Ei.atom('page'), 
					     pagingOptions.currentPage),
				    Ei.tuple(Ei.atom('lastpage'), 
					     selectOptions.lastPage),
				    Ei.tuple(Ei.atom('lastid'), 
					     selectOptions.lastId),
				    Ei.tuple(Ei.atom('match'), 
					     filterOptions.filterText)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseListReply(reply, callback);
			 });
	    }, 100);
	};
	
	return {
	    users: users,
	    getData: getData
	};
	
    }]);

exowebUserServices.factory('UserDetail', ['ExowebError',
    function(ExowebError) {
	var user = new Object;
	
	var displayUser = function (attArray, callback) {
	    attArray = Wse.decode_js(attArray);
	    var i, attArray;
	    // Loop over atttribute objects and set display fields
	    for (i=0; i < attArray.length; i++){
		window.console.debug("Attribute = " +attArray[i].name);
		window.console.debug("Value = " +attArray[i].val);
		switch(attArray[i].name) {
		case "email": user.email = attArray[i].val; break;
		case "phone": user.phone = attArray[i].val; break;
		case "role": user.role = attArray[i].val; break;
		default: window.console.debug("Unknown attribute = " +attArray[i].name);
		}
	    }
	    callback();
	};

	var parseDetailReply = function(reply, callback) {
	  if (reply.value[0] == "ok") {		       
	      if (reply.value[0] == "ok") {
		  // Call performed
		  var result = reply.value[1];
		  window.console.debug("Result = " +result.value[1]);
		  if (result.value[0] == "ok") {
		      // call successful => {ok,{ok, User}}
		      window.console.debug("User = " +result.value[1]);
		      displayUser(result.value[1], callback);
		      }
		  else if (result.value[0] == "error") {
		      // Call failed => {ok,{error,Reason}}
		      ExowebError(result.value[1]);
		  }
	      }
	      else if (reply.value[0] == "error") {
		  // Call not performed => {error, Reason}
		  window.alert("Error: " +reply.value[1]);
	      }
	  };
	};

	
	var getData = function(username, callback) {
	    user.username = username;
	    Wse.call('exoweb_js', 'wrapper', 
		     [Ei.atom('exoweb_user'),  // Module
		      Ei.atom('event'),          // Function
		      Ei.tuple(Ei.atom('select'), // Args
			       [Ei.tuple(Ei.atom('name'), username)])], 
		     // reply callback
		     function(obj,ref,reply) {  
			 window.console.debug("Value = " +reply);
			 parseDetailReply(reply, callback);
		     });
	}

	return {
	    user: user,
	    getData: getData
	}
    }]);
		
exowebUserServices.factory('User', ['ExowebError',
    function(ExowebError) {

	var parseUserReply = function(reply, user, callback) {
	    if (reply.value[0] == "ok") {
		// Call performed
		var result = reply.value[1];
		window.console.debug("Result = " + result);
		if (result == "ok") {
		    // call successful => {ok, ok}
		    callback(user);
		}
		else if (result.value[0] == "error") {
		    // Call failed => {ok,{error,Reason}}
		    ExowebError(result.value[1]);
		}
	    }
	    else if (reply.value[0] == "error") {
		// Call not performed => {error, Reason}
		window.alert("Error: " +reply.value[1]);
	    }};

	var update = function(user, callback) {
	    setTimeout(function () {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_user'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('update'), // Args
				   [Ei.tuple(Ei.atom('name'), 
					     user.name),
				    Ei.tuple(Ei.atom('email'), 
					     user.email),
				    Ei.tuple(Ei.atom('password'), 
					     user.password),
				    Ei.tuple(Ei.atom('role'), 
					     user.role), 
				    Ei.tuple(Ei.atom('phone'), 
					     user.phone), 
				    Ei.tuple(Ei.atom('delete'), 
					     user.deleteuser)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseUserReply(reply, user, callback);
			 });
	    }, 100);
	}

	var create = function(user, callback) {
	    setTimeout(function () {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_user'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('create'), // Args
				   [Ei.tuple(Ei.atom('name'), 
					     user.name),
				    Ei.tuple(Ei.atom('email'), 
					     user.email),
				    Ei.tuple(Ei.atom('password'), 
					     user.password),
				    Ei.tuple(Ei.atom('role'), 
					     user.role), 
				    Ei.tuple(Ei.atom('phone'), 
					     user.phone)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseUserReply(reply, user, callback);
			 });
	    }, 100);
	}

	return {
	    update: update,
	    create: create
	}
    }]);

		
	