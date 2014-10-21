'use strict';

/* Controllers */

var exowebControllers = 
    angular.module('exowebControllers', ['ngRoute', 'ngGrid']);


exowebControllers.controller('ApplyCtrl', ['$scope',
  function($scope) {
      
      $scope.apply = function(user) {
	  window.console.debug("User = " +user);
	  Wse.call('exoweb_apply', 'event', 
		   [Ei.tuple(Ei.atom('send'),
		    [Ei.tuple(Ei.atom('email'), user.email),
		     Ei.tuple(Ei.atom('password'), user.password)])], 
		   // reply callback
		   function(obj,ref,reply) {  
		       window.console.debug("Value = " +reply);
		       if (reply == "{ok, ok}") {
			   window.alert("Confirm mail sent to " +user.email);}
		       else if (reply.value[0] == "error") {
			   // Call not performed => {error, Reason}
			   window.alert("Error: " +reply.value[1]);
		       }})};

      $scope.passwordConfirmed = function(user) {
	  if (user.password != user.confirmpassword) {
	      window.alert("Passwords do not match! ");}
	  return angular.equals(user.password, user.confirmpassword)};

  }]);


exowebControllers.controller('ConfirmCtrl', 
			     ['$scope', '$routeParams',
			      function($scope, $routeParams) {
      $scope.account = $routeParams.account;
      $scope.email = $routeParams.email;
      $scope.session = $routeParams.session;
      
      $scope.confirm = function() {
	  Wse.call('exoweb_confirm', 'event', 
		   [Ei.tuple(Ei.atom('confirm'),
		    [Ei.tuple(Ei.atom('account'), $scope.account),
		     Ei.tuple(Ei.atom('email'), $scope.email),
		     Ei.tuple(Ei.atom('session'), $scope.session)])], 
		   // reply callback
		   function(obj,ref,reply) {
		       window.console.debug("Value = " +reply);
		       parseReply(reply, user);
		       window.console.debug("Reply = " +reply);
		       window.console.debug("Value 0 = " +reply.value[0]);
		       window.console.debug("Value 1 = " +reply.value[1]);
		   })};
				  
      var parseReply = function(reply, user) {
	  if (reply.value[0] == "ok") {		       
	      if (reply.value[0] == "ok") {
		  // Call performed
		  var result = reply.value[1];
		  window.console.debug("Result = " +result.value[1]);
		  if (result.value[0] == "ok") {
		      // call successful => {ok,{ok,LoginName}}
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
	      }}};
  }]);


exowebControllers.controller('LoginCtrl', ['$scope',
  function($scope) {
      
      $scope.login = function(user) {
	  window.console.debug("User = " +user);
	  Wse.call('exoweb_login', 'event', 
		   [Ei.tuple(Ei.atom('login'),
		    [Ei.tuple(Ei.atom('name'), user.name),
		     Ei.tuple(Ei.atom('password'), user.password)])], 
		   // reply callback
		   function(obj,ref,reply) {  
		       window.console.debug("Value = " +reply);
		       parseReply(reply, user);
		   })};

     var parseReply = function(reply, user) {
	  if (reply.value[0] == "ok") {
	      // Call performed
	      var result = reply.value[1];
	      window.console.debug("Result = " +result);
	      if (result == "ok") {
		  // call successful => {ok, ok}
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
	  }};

      var createCookie = function(user) {
	  Wse.start('exoweb_js', 'create_cookie', 
		    [[Ei.tuple(Ei.atom('name'), user.name),
		      Ei.tuple(Ei.atom('password'), user.password),
		      Ei.tuple(Ei.atom('path'), "#/device")]])};


  }]);

exowebControllers.controller('MenuCtrl', ['$scope', '$routeParams',
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
	  else {
	      Wse.call('exoweb_js', 'wrapper', 
		       [Ei.atom('exoweb_login'),  // Module
			Ei.atom('event'),          // Function
			Ei.tuple(Ei.atom('user'),[])], // Args
			// reply callback
			function(obj,ref,reply) {  
			    window.console.debug("User = " +reply);
			    parseUserReply(reply);
			})
	  }};

      var parseUserReply = function(reply) {
	  if (reply.value[0] == "ok") {		       
	      if (reply.value[0] == "ok") {
		  // Call performed
		  var result = reply.value[1];
		  window.console.debug("Result = " +result.value[1]);
		  if (result.value[0] == "ok") {
		      // call successful => {ok,{ok,User}}
		      $scope.user = result.value[1];
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
	  }};

      // Called when user clicks logout-button
      $scope.logout = function() {
	  var cookie = document.cookie;
 	  window.console.debug("Cookie before reset " +cookie);
	  document.cookie="id=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/";
	  cookie = document.cookie;
 	  window.console.debug("Cookie after reset " +cookie);
	  deleteCookie();
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

      var deleteCookie = function() {
	  Wse.call('exoweb_js', 'delete_cookie',  [], 
		   function(obj,ref,reply) {  
		       window.console.debug("Value = " +reply);
		   })};

       // Call this at load
      redirect();

      }
  ]);


exowebControllers.controller('DeviceListCtrl', ['$scope', '$http',
    function($scope, $http) {
	$scope.totalServerItems = 0;
	$scope.filterOptions = {
            filterText: "",
            useExternalFilter: false
	}; 
	$scope.pagingOptions = {
            pageSizes: [10, 20, 50],
            pageSize: 10,
            currentPage: 1,
            lastPage: 0,
            lastId: ""
	};	

	$scope.getData = 
	    function () {
		setTimeout(function () {
		    Wse.call('exoweb_js', 'wrapper', 
			     [Ei.atom('exoweb_device'),  // Module
			      Ei.atom('event'),          // Function
			      Ei.tuple(Ei.atom('load'), // Args
				[Ei.tuple(Ei.atom('rows'), 
					 $scope.pagingOptions.pageSize),
				 Ei.tuple(Ei.atom('page'), 
					  $scope.pagingOptions.currentPage),
				 Ei.tuple(Ei.atom('lastpage'), 
					  $scope.pagingOptions.lastPage),
				 Ei.tuple(Ei.atom('lastid'), 
					  $scope.pagingOptions.lastId),
				 Ei.tuple(Ei.atom('match'), 
					  $scope.filterOptions.filterText)])], 
			     // reply callback
			     function(obj,ref,reply) {  
				 window.console.debug("Value = " +reply);
				 parseListReply(reply);
			     });
		}, 100);
	    };
	
	$scope.setPageData = function(data){	
            $scope.myData = data;
            $scope.totalServerItems = data.length;
            if (!$scope.$$phase) {
		$scope.$apply();
            }
	};

	var parseListReply = function(reply) {
	  if (reply.value[0] == "ok") {		       
	      if (reply.value[0] == "ok") {
		  // Call performed
		  var result = reply.value[1];
		  window.console.debug("Result = " +result.value[1]);
		  if (result.value[0] == "ok") {
		      // call successful => {ok,{ok, Data}}
		      fillTable(result.value[1]);
		  }
		  else if (result.value[0] == "error") {
		      // Call failed => {ok,{error,Reason}}
		      exowebError(result.value[1]);
		  }
	      }
	      else if (reply.value[0] == "error") {
		  // Call not performed => {error, Reason}
		  window.alert("Error: " +reply.value[1]);
	      }
	  };
	};

	var fillTable = function(devArray) {
	    var i;
	    // Loop over devices
	    for (i=0; i < devArray.length; i++){
		var j, attArray;
		var device = new Object;
		devArray[i] = Wse.decode_js(devArray[i]);
		attArray =  devArray[i].attributes;
		device["id"] = (devArray[i])["device-id"];
		// Loop over atttributes
		for (j=0; j < attArray.length; j++)
		    device[attArray[j].name] = attArray[j].val;
		// Make status attribute understandable
		window.console.debug("is-connected = " +device["is-connected"]);
		if (device["is-connected"] == "true") 
		    device["is-connected"] = "Connected";
		else
		     device["is-connected"] = "Not connected";
		window.console.debug("Status = " +device["is-connected"]);
		devArray[i] = device;
	    }
	    
	    $scope.setPageData(devArray);
	    $scope.pagingOptions.lastPage = $scope.pagingOptions.currentPage;
	    $scope.pagingOptions.lastId = (devArray[devArray.length - 1])["id"];
	    window.console.debug("Last = " + $scope.pagingOptions.lastId);
	    window.console.debug("Last page = " + $scope.pagingOptions.lastPage);

	};


	
	var rowSelected = function(rowItem, event) {
	    $scope.deviceid = rowItem.getProperty('id');
	    window.console.debug("Row = " +rowItem.rowIndex);
	    window.console.debug("Event = " +event);
	    window.console.debug("Id = " +$scope.deviceid);
	    Wse.call('exoweb_js', 'wrapper', 
		     [Ei.atom('exoweb_device'),  // Module
		      Ei.atom('event'),          // Function
		      Ei.tuple(Ei.atom('select'), // Args
				[Ei.tuple(Ei.atom('device-id'), 
					  rowItem.getProperty('id'))])], 
			     // reply callback
			     function(obj,ref,reply) {  
				 window.console.debug("Value = " +reply);
				 parseDeviceReply(reply);
			     });
	};
	    
	var parseDeviceReply = function(reply) {
	  if (reply.value[0] == "ok") {		       
	      if (reply.value[0] == "ok") {
		  // Call performed
		  var result = reply.value[1];
		  window.console.debug("Result = " +result.value[1]);
		  if (result.value[0] == "ok") {
		      // call successful => {ok,{ok, Device}}
		      window.console.debug("Device = " +result.value[1]);
		      displayDevice(result.value[1]);
		      }
		  else if (result.value[0] == "error") {
		      // Call failed => {ok,{error,Reason}}
		      exowebError(result.value[1]);
		  }
	      }
	      else if (reply.value[0] == "error") {
		  // Call not performed => {error, Reason}
		  window.alert("Error: " +reply.value[1]);
	      }
	  };
	};

	var displayDevice = function (attArray) {
	    attArray = Wse.decode_js(attArray);
	    window.console.debug("Device = " +$scope.deviceid);
	    var i, attArray;
	    var device = new Object;
	    // Loop over atttribute obiects
	    for (i=0; i < attArray.length; i++){
		window.console.debug("Attribute = " +attArray[i].name);
		window.console.debug("Value = " +attArray[i].val);
		switch(attArray[i].name) {
		case "msisdn": $scope.msisdn = attArray[i].val; break;
		case "device-key": $scope.devicekey = attArray[i].val; break;
		case "server-key": $scope.serverkey = attArray[i].val; break;
		case "is-connected": 
		    if (attArray[i].val == "true")
			$scope.status = "Connected";
		    else if (attArray[i].val == "false")
			$scope.status = "Not connected"; 
		    break;
		default: window.console.debug("Unknown attribute = " +attArray[i].name);
		}
	    }
	    $scope.$apply();
	};

	var exowebError = function(error) {
	    if (error == "illegal_cookie") {
		window.console.debug("Go to login");
		window.location.href = "#/login";
	    }
	    else {
		window.alert("Error: " +result.value[1]);
	    }};

	$scope.getData();
	
	$scope.$watch('pagingOptions', function (newVal, oldVal) {
            if (newVal !== oldVal && 
		newVal.currentPage !== oldVal.currentPage) {
		if (newVal.pageSize !== oldVal.pageSize) {
		    newVal.currentPage = 1;
		}
		window.console.debug("paging changed ");
		window.console.debug("Last = " + $scope.pagingOptions.last);
		window.console.debug("Last page = " + $scope.pagingOptions.lastPage);		$scope.getData();
            }
	}, true);

	$scope.$watch('filterOptions', function (newVal, oldVal) {
            if (newVal !== oldVal) {
		$scope.getData();
            }
	}, true);
	
	// This $watch scrolls the ngGrid to show a newly-selected row as 
	// close to the middle row as possible
	$scope.$watch('gridOptions.ngGrid.config.selectedItems', 
	    function (newValue, oldValue, scope) {
		if (newValue != oldValue && newValue.length > 0) {
		    var grid = scope.gridOptions.ngGrid;
		    var rowIndex = grid.data.indexOf(newValue[0]);
		    grid.$viewport.scrollTop(Math.max(0, (rowIndex - 4))*grid.config.rowHeight);
		}
	    }, true);
	
	$scope.gridOptions = {
            data: 'myData',
 	    columnDefs: [{field:'id', displayName:'My devices', width: 100}, 
			 {field:'is-connected', displayName:'Status', width: 100}],
            totalServerItems: 'totalServerItems',
            pagingOptions: $scope.pagingOptions,
            filterOptions: $scope.filterOptions,
            enablePaging: true,
	    showFooter: true,
	    keepLastSelected: false,
	    enableSorting: false,
	    enableCellSelection: true,
	    selectedItems: $scope.mySelections,
	    afterSelectionChange: rowSelected,
	    multiSelect: false
	};
    }]);


exowebControllers.controller('ReadTabCtrl', ['$scope', 
    function ($scope) {
	window.console.debug('Loading ReadTabCtrl');
	$scope.title = 'Read';
    }
]);

exowebControllers.controller('EditTabCtrl', ['$scope', 
    function ($scope) {
	window.console.debug('Loading EditTabCtrl');
	$scope.connect = false;
	$scope.deletequeue = false;
	$scope.deletedevice = false;

	$scope.update = function (device) {
	    if (device.connect == undefined) device.connect = false;
	    if (device.deletequeue == undefined) device.deletequeue = false;
	    if (device.deletedevice == undefined) device.deletedevice = false;
	    window.console.debug("Device = " +device);
	    window.console.debug("Device key = " +device.dkey);
	    window.console.debug("Server key = " +device.skey);
	    window.console.debug("MsIsdn = " +device.msisdn);
	    window.console.debug("Delete = " +device.deletedevice);
	    setTimeout(function () {
		Wse.call('exoweb_js', 'wrapper', 
			 [Ei.atom('exoweb_device'),  // Module
			  Ei.atom('event'),          // Function
			  Ei.tuple(Ei.atom('update'), // Args
				   [Ei.tuple(Ei.atom('device-id'), 
					     $scope.deviceid),
				    Ei.tuple(Ei.atom('device-key'), 
					     device.dkey),
				    Ei.tuple(Ei.atom('server-key'), 
					     device.skey),
				    Ei.tuple(Ei.atom('msisdn'), 
					     device.msisdn),
				    Ei.tuple(Ei.atom('delete'), 
					     device.deletedevice)])], 
			 // reply callback
			 function(obj,ref,reply) {  
			     window.console.debug("Value = " +reply);
			     //parseListReply(reply);
			 });
	    }, 100);
	};
	
    }


]);
