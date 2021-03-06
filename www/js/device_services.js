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
// Exoweb device services
//
// Author: Marina Westman L�nne
// Created: October 2014
//
//----------------------------------------------------------------------------
'use strict';


var exowebDeviceServices = angular.module('exowebDeviceServices', []);


exowebDeviceServices.factory('DeviceList', ['ExowebError',
    function(ExowebError) {

	var devices = [];

	function fillTable(dataArray, callback) {
	    var i;
	    // Loop over devices
	    for (i=0; i < dataArray.length; i++){
		var j, attArray;
		var device = new Object;
		dataArray[i] = Wse.decode_js(dataArray[i]);
		attArray =  dataArray[i].attributes;
		device["id"] = (dataArray[i])["device-id"];
		// Loop over atttributes
		for (j=0; j < attArray.length; j++)
		    device[attArray[j].name] = attArray[j].val;
		// Make status attribute understandable
		// Can't use field with "-" in name in ng-grid
		if (device["is-connected"] == "true") 
		     device["status"] = "Connected";
		 else
		      device["status"] = "Not connected";
		// Add fields not yet available
		device["changed"] = "00-00-00";
		device["created"] = "00-00-00";
		device["inqueue"] = "Unknown";

		/// For test
		if (device["id"] == 200) device["reserved"] = 2189378;

		if (device["reserved"] == exodmSession)
		    device["locked"] = false;
		else if (device["reserved"] !== undefined) // Which value ??
		    device["locked"] = true;
		else
		    device["locked"] = false;
		devices[i] = device;
		window.console.debug("Device " + i + " = " + 
				     JSON.stringify(devices[i]));
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
		devices.splice(0, devices.length);
		window.console.debug("getData");
		window.console.debug("Current page = " + pagingOptions.currentPage);
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_device'),  // Module
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
	    devices: devices,
	    getData: getData
	};
	
    }]);

exowebDeviceServices.factory('DeviceDetail', ['ExowebError',
    function(ExowebError) {
	var device = new Object;
	
	var displayDevice = function (attArray, callback) {
	    attArray = Wse.decode_js(attArray);
	    var i, attArray;
	    // Loop over atttribute objects and set display fields
	    for (i=0; i < attArray.length; i++){
		window.console.debug("Attribute = " +attArray[i].name);
		window.console.debug("Value = " +attArray[i].val);
		switch(attArray[i].name) {
		case "msisdn": device.msisdn = attArray[i].val; break;
		case "device-key": device.dkey = attArray[i].val; break;
		case "server-key": device.skey = attArray[i].val; break;
		case "is-connected": 
		    if (attArray[i].val == "true")
			device.status = "Connected";
		    else if (attArray[i].val == "false")
			device.status = "Not connected"; 
		    break;
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
		      // call successful => {ok,{ok, Device}}
		      window.console.debug("Device = " +result.value[1]);
		      displayDevice(result.value[1], callback);
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
		      window.console.debug("Device call ok");
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
	
	var getData = function(deviceid, callback) {
	    device.did = deviceid;
	    Wse.call('exoweb_js', 'wrapper', 
		     [Ei.atom('exoweb_device'),  // Module
		      Ei.atom('event'),          // Function
		      Ei.tuple(Ei.atom('select'), // Args
			       [Ei.tuple(Ei.atom('device-id'), deviceid)])], 
		     // reply callback
		     function(obj,ref,reply) {  
			 window.console.debug("Value = " +reply);
			 parseDetailReply(reply, callback);
		     });
	}

	var reserve = function(deviceid) {
	    Wse.call('exoweb_js', 'wrapper', 
		     [Ei.atom('exoweb_device'),  // Module
		      Ei.atom('event'),          // Function
		      Ei.tuple(Ei.atom('reserve'), // Args
			       [Ei.tuple(Ei.atom('device-id'), deviceid),
				Ei.tuple(Ei.atom('session'), exodmSession)])], 
		     // reply callback
		     function(obj,ref,reply) {  
			 window.console.debug("Value = " +reply);
			 parseReply(reply);
		     });
	}
	    
	var release = function(deviceid) {
	    Wse.call('exoweb_js', 'wrapper', 
		     [Ei.atom('exoweb_device'),  // Module
		      Ei.atom('event'),          // Function
		      Ei.tuple(Ei.atom('release'), // Args
			       [Ei.tuple(Ei.atom('device-id'), deviceid),
				Ei.tuple(Ei.atom('session'), exodmSession)])], 
		     // reply callback
		     function(obj,ref,reply) {  
			 window.console.debug("Value = " +reply);
			 parseReply(reply);
		     });
	}

	return {
	    device: device,
	    getData: getData,
	    reserve: reserve,
	    release: release
	}
    }]);
		
exowebDeviceServices.factory('Device', ['ExowebError',
    function(ExowebError) {

	var parseDeviceReply = function(reply, device, callback) {
	    if (reply.value[0] == "ok") {
		// Call performed
		var result = reply.value[1];
		window.console.debug("Result = " + result);
		if (result == "ok") {
		    // call successful => {ok, ok}
		    window.console.debug("Call successful.");
		    callback(device);
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

	var update = function(device, callback) {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_device'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('update'), // Args
				   [Ei.tuple(Ei.atom('device-id'), 
					     device.did),
				    Ei.tuple(Ei.atom('device-key'), 
					     device.dkey),
				    Ei.tuple(Ei.atom('server-key'), 
					     device.skey),
				    Ei.tuple(Ei.atom('msisdn'), 
					     device.msisdn)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseDeviceReply(reply, device, callback);
			 });
	}

	var remove = function(device, callback) {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_device'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('delete'), // Args
				   [Ei.tuple(Ei.atom('device-id'), 
					     device.did)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseDeviceReply(reply, device, callback);
			 });
	}

	var create = function(device, callback) {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_device'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('create'), // Args
				   [Ei.tuple(Ei.atom('device-id'), 
					     device.did),
				    Ei.tuple(Ei.atom('device-key'), 
					     device.dkey),
				    Ei.tuple(Ei.atom('server-key'), 
					     device.skey),
				    Ei.tuple(Ei.atom('msisdn'), 
					     device.msisdn)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     parseDeviceReply(reply, device, callback);
			 });
	}

	return {
	    update: update,
	    remove: remove,
	    create: create
	}
    }]);

	