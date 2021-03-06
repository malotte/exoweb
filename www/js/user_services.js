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
// Exoweb user services
//
// Author: Marina Westman L�nne
// Created: October 2014
//
//----------------------------------------------------------------------------

'use strict';


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
		roleArray =  dataArray[i].roles;
		user["name"] = (dataArray[i])["name"];
		// Get first role
		user["role"] = roleArray[0];
		// Add fields not yet available
		user["changed"] = "00-00-00";
		user["created"] = "00-00-00";
		/// For test

		if (user["name"] == 't') user["reserved"] = 2189378;

		if (user["reserved"] == exodmSession)
		    user["locked"] = false;
		else if (user["reserved"] !== undefined) // Which value ??
		    user["locked"] = true;
		else
		    user["locked"] = false;
		users[i] = user;

		window.console.debug("User " + i + " = " + JSON.stringify(users[i]));
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
	    window.console.debug("Attributes = " + JSON.stringify(attArray));
	    // Loop over atttribute objects and set display fields
	    for (i=0; i < attArray.length; i++){
		window.console.debug("Attribute = " +
				     JSON.stringify(attArray[i]));
		window.console.debug("Email = " +attArray[i].email);
		window.console.debug("Phone = " +attArray[i].phone);
		window.console.debug("Role = " +attArray[i].role);
		if (attArray[i].email !== undefined) 
		    user.email = attArray[i].email;
		if (attArray[i].phone !== undefined) 
		    user.phone = attArray[i].phone;
		if (attArray[i].role !== undefined) 
		    user.role = attArray[i].role;
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

	var parseReply = function(reply) {
	  if (reply.value[0] == "ok") {		       
	      if (reply.value[0] == "ok") {
		  // Call performed
		  var result = reply.value[1];
		  window.console.debug("Result = " +result.value[1]);
		  if (result.value[0] == "ok") {
		      // call successful => {ok, ok}
		      window.console.debug("User call ok");
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
	    user.name = username;
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

	var reserve = function(username) {
	    Wse.call('exoweb_js', 'wrapper', 
		     [Ei.atom('exoweb_user'),  // Module
		      Ei.atom('event'),          // Function
		      Ei.tuple(Ei.atom('reserve'), // Args
			       [Ei.tuple(Ei.atom('name'), username),
				Ei.tuple(Ei.atom('session'), exodmSession)])], 
		     // reply callback
		     function(obj,ref,reply) {  
			 window.console.debug("Value = " +reply);
			 parseReply(reply);
		     });
	}
	    
	var release = function(username) {
	    Wse.call('exoweb_js', 'wrapper', 
		     [Ei.atom('exoweb_user'),  // Module
		      Ei.atom('event'),          // Function
		      Ei.tuple(Ei.atom('release'), // Args
			       [Ei.tuple(Ei.atom('name'), username),
				Ei.tuple(Ei.atom('session'), exodmSession)])], 
		     // reply callback
		     function(obj,ref,reply) {  
			 window.console.debug("Value = " +reply);
			 parseReply(reply);
		     });
	}
	return {
	    user: user,
	    getData: getData,
	    reserve: reserve,
	    release: release
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
	    window.console.debug("User to update = " + JSON.stringify(user));
	    Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_user'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('update'), // Args
				   [Ei.tuple(Ei.atom('name'), 
					     user.name),
				    Ei.tuple(Ei.atom('email'), 
					     user.email),
				    Ei.tuple(Ei.atom('role'), 
					     user.role), 
				    Ei.tuple(Ei.atom('phone'), 
					     user.phone)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseUserReply(reply, user, callback);
			 });
	}

	var changepassword = function(user, callback) {
	    window.console.debug("User to change password for = " +
				 JSON.stringify(user));
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_user'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('update'), // Args
				   [Ei.tuple(Ei.atom('name'), 
					     user.name),
				    Ei.tuple(Ei.atom('password'), 
					     user.password)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseUserReply(reply, user, callback);
			 });
	}

	var remove = function(user, callback) {
	    window.console.debug("User to delete = " + JSON.stringify(user));
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_user'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('delete'), // Args
				   [Ei.tuple(Ei.atom('name'), 
					     user.name)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseUserReply(reply, user, callback);
			 });
	}

	var create = function(user, callback) {
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
	}

	return {
	    update: update,
	    changepassword: changepassword,
	    remove: remove,
	    create: create
	}
    }]);

		
	