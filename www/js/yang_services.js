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
// Exoweb yang file services
//
// Author: Marina Westman L�nne
// Created: October 2014
//
//----------------------------------------------------------------------------

'use strict';

var exowebYangServices = angular.module('exowebYangServices', []);


exowebYangServices.factory('YangList', ['ExowebError',
    function(ExowebError) {

	var yangs = [];

	function fillTable(dataArray, callback) {
	    var i;
	    // Loop over yangs
	    for (i=0; i < dataArray.length; i++){
		var j, roleArray;
		var yang = new Object;
		dataArray[i] = Wse.decode_js(dataArray[i]);
		window.console.debug("Yang " + i + " = " + dataArray[i]);
		yang["filename"] = dataArray[i];
		// Add fields not yet available
		yang["created"] = "00-00-00";
		yangs[i] = yang;
		window.console.debug("Yang " + i + " = " + yangs[i]);
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
		yangs.splice(0, yangs.length);
		window.console.debug("Last = " + selectOptions.lastId);
		window.console.debug("Last page = " + selectOptions.lastPage);
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_yang'),  // Module
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
	    yangs: yangs,
	    getData: getData
	};
	
    }]);

exowebYangServices.factory('YangDetail', ['ExowebError',
    function(ExowebError) {
	var yang = new Object;
	
	var displayYang = function (attArray, callback) {
	    callback();
	};

	var parseDetailReply = function(reply, callback) {
	  if (reply.value[0] == "ok") {		       
	      if (reply.value[0] == "ok") {
		  // Call performed
		  var result = reply.value[1];
		  window.console.debug("Result = " +result.value[1]);
		  if (result.value[0] == "ok") {
		      // call successful => {ok,{ok, Yang}}
		      window.console.debug("Yang = " +result.value[1]);
		      displayYang(result.value[1], callback);
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

	
	var getData = function(filename, callback) {
	    yang.filename = filename;
	    Wse.call('exoweb_js', 'wrapper', 
		     [Ei.atom('exoweb_yang'),  // Module
		      Ei.atom('event'),          // Function
		      Ei.tuple(Ei.atom('select'), // Args
			       [Ei.tuple(Ei.atom('filename'), filename)])], 
		     // reply callback
		     function(obj,ref,reply) {  
			 window.console.debug("Value = " +reply);
			 parseDetailReply(reply, callback);
		     });
	}

	return {
	    yang: yang,
	    getData: getData
	}
    }]);
		
exowebYangServices.factory('Yang', [
    '$http', 'ExowebError',
    function($http, ExowebError) {
	
	var parseYangReply = function(reply, yang, callback) {
	    if (reply.value[0] == "ok") {
		// Call performed
		var result = reply.value[1];
		window.console.debug("Result = " + result);
		if (result == "ok") {
		    // call successful => {ok, ok}
		    callback(yang);
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

	var remove = function(yang, callback) {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_yang'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('delete'), // Args
				   [Ei.tuple(Ei.atom('filename'), 
					     yang)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseYangReply(reply, yang, callback);
			 });
	}

	var create = function(yang, callback) {
	    var url = "/fileupload";
	    var fd = new FormData();
	    fd.append('file', yang.filename);
	    $http.post(url, fd, {
		transformRequest: angular.identity,
		headers: {'Content-Type': undefined},
	    });
	    callback(yang);
	}

	return {
	    remove: remove,
	    create: create
	}
    }]);

		
	