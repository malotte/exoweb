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
		       parseLoginReply(reply, user);
		   })};

     var parseLoginReply = function(reply, user) {
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

exowebControllers.controller('MenuCtrl', 
			     ['$scope', '$routeParams', 'Redirect',
     function($scope, $routeParams, Redirect) {
      
	 // If not logged in redirect
	 //$scope.$on('$routeChangeSuccess', function () {
	 
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

	 var deleteCookie = function() {
	     Wse.call('exoweb_js', 'delete_cookie',  [], 
		      function(obj,ref,reply) {  
			  window.console.debug("Value = " +reply);
		      })};
	 
	 // Call this at load
	 Redirect($scope.user);

     }]);


exowebControllers.controller('DeviceListCtrl', [
    '$scope', 'DeviceList', 'DeviceDetail',
    function($scope, DeviceList, DeviceDetail) {
	
	var scroll = function(rowItem, event){
           if(!event.ctrlKey && !event.shiftKey && event.type != 'click'){
		var grid = $scope.gridOptions.ngGrid;
		grid.$viewport.scrollTop(rowItem.offsetTop - (grid.config.rowHeight * 2));
		angular.forEach($scope.myData, function(data, index){
		    $scope.gridOptions.selectRow(index, false);
		});
            }
            return true;
	}
	
	var listCallback = function() {
	    var devices = DeviceList.devices;
	    window.console.debug("devices = " + devices);
	    $scope.setPageData(devices);
	    $scope.selectOptions.lastPage = $scope.pagingOptions.currentPage;
	    $scope.selectOptions.lastId = (devices[devices.length - 1])["id"];
	    window.console.debug("Total = " + $scope.totalServerItems);
	    window.console.debug("Total = " + $scope.gridOptions.totalServerItems);
	    window.console.debug("Last = " + $scope.selectOptions.lastId);
	    window.console.debug("Last page = " + $scope.selectOptions.lastPage);
	};

	var rowSelected = function(rowItem, event) {
	    $scope.deviceid = rowItem.getProperty('id');
	    window.console.debug("Row = " +rowItem.rowIndex);
	    window.console.debug("Event = " +event);
	    window.console.debug("Id = " +$scope.deviceid);
	    DeviceDetail.getData($scope.deviceid, detailCallback);
	};
	    
	var detailCallback = function() {
	    var device = DeviceDetail.device;
	    window.console.debug("device = " + device);
	    $scope.status = device.status;
	    $scope.serverkey = device.serverkey;
	    $scope.devicekey = device.devicekey;
	    $scope.msisdn = device.msisdn;	    
	    $scope.$apply();
	}
	$scope.setPageData = function(data){
	    // These variables are watched by ng-grid
	    $scope.myData = data;
	    $scope.totalServerItems = 10;
	    if (!$scope.$$phase) {
		$scope.$apply();
	    }
	};
	

	$scope.totalServerItems = 0;
	$scope.pagingOptions = {
            pageSizes: [10, 20, 50],
            pageSize: 10,
            currentPage: 1
	};	
 	$scope.selectOptions = {
            lastPage: 0,
            lastId: ""
	};	
	$scope.filterOptions = {
            filterText: "",
            useExternalFilter: false
	}; 
						    
	DeviceList.getData($scope.pagingOptions, 
			   $scope.selectOptions, 
			   $scope.filterOptions,
			   listCallback);


	
	$scope.$watch('pagingOptions', function (newVal, oldVal) {
            if (newVal !== oldVal && 
		newVal.currentPage !== oldVal.currentPage) {
		if (newVal.pageSize !== oldVal.pageSize) {
		    newVal.currentPage = 1;
		}
		window.console.debug("paging changed ");
		window.console.debug("Last = " + $scope.pagingOptions.last);
		window.console.debug("Last page = " + $scope.pagingOptions.lastPage);
		DeviceList.getData($scope.pagingOptions, 
				   $scope.selectOptions, 
				   $scope.filterOptions,
				   listCallback);
            }
	}, true);

	$scope.$watch('filterOptions', function (newVal, oldVal) {
            if (newVal !== oldVal) {
		DeviceList.getData($scope.pagingOptions, 
				   $scope.selectOptions, 
				   $scope.filterOptions,
				   listCallback);
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
            data: 'myData',  // Watch this variable
	    primaryKey: 'id',
 	    columnDefs: [{field:'id', displayName:'My devices', width: 100}, 
			 {field:'status', displayName:'Status', width: 100}],
            totalServerItems: 'totalServerItems', // Watch this variable
            pagingOptions: $scope.pagingOptions,
            filterOptions: $scope.filterOptions,
            enablePaging: true,
	    showFooter: true,
	    keepLastSelected: false,
	    enableSorting: false,
	    enableCellSelection: true,
	    selectedItems: $scope.mySelections,
	    beforeSelectionChange: scroll,
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
			     parseEditReply(reply, $scope.deviceid);
			 });
	    }, 100);
	};
	
	var parseEditReply = function(reply, did) {
	    if (reply.value[0] == "ok") {
		// Call performed
		var result = reply.value[1];
		window.console.debug("Result = " + result);
		if (result == "ok") {
		    // call successful => {ok, ok}
		    window.alert("Device" + did + " updated");
		}
		else if (result.value[0] == "error") {
		    // Call failed => {ok,{error,Reason}}
		    window.alert("Error: " + result.value[1]);
		}
	    }
	    else if (reply.value[0] == "error") {
		// Call not performed => {error, Reason}
		window.alert("Error: " +reply.value[1]);
	    }};

    }
]);

exowebControllers.controller('CreateTabCtrl', ['$scope', 
    function ($scope) {
	window.console.debug('Loading CreateTabCtrl');	

	$scope.create = function (device) {
	    window.console.debug("Device = " +device);
	    window.console.debug("Device id = " +device.did);
	    window.console.debug("Device key = " +device.dkey);
	    window.console.debug("Server key = " +device.skey);
	    window.console.debug("MsIsdn = " +device.msisdn);
	    setTimeout(function () {
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
			     parseCreateReply(reply, device);
			 });
	    }, 100);
	};
	
	var parseCreateReply = function(reply, device) {
	    if (reply.value[0] == "ok") {
		// Call performed
		var result = reply.value[1];
		window.console.debug("Result = " + result);
		if (result == "ok") {
		    // call successful => {ok, ok}
		    window.alert("Device " + device.did + " created");
		}
		else if (result.value[0] == "error") {
		    // Call failed => {ok,{error,Reason}}
		    window.alert("Error: " + result.value[1]);
		}
	    }
	    else if (reply.value[0] == "error") {
		// Call not performed => {error, Reason}
		window.alert("Error: " +reply.value[1]);
	    }};

    }
]);
