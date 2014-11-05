'use strict';

/* Services */

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
		setTimeout(function () {
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
	    }, 100);
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
		
exowebYangServices.factory('Yang', ['ExowebError',
    function(ExowebError) {

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

	var update = function(yang, callback) {
	    setTimeout(function () {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_yang'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('update'), // Args
				   [Ei.tuple(Ei.atom('filename'), 
					     yang.name),
				    Ei.tuple(Ei.atom('delete'), 
					     yang.deletefile)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseYangReply(reply, yang, callback);
			 });
	    }, 100);
	}

	var create = function(yang, callback) {
   	    setTimeout(function () {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_yang'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('create'), // Args
				   [Ei.tuple(Ei.atom('filename'), 
					     yang.filename)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseYangReply(reply, yang, callback);
			 });
	    }, 100);
	}

	return {
	    update: update,
	    create: create
	}
    }]);

		
	