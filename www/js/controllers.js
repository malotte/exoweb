'use strict';

/* Controllers */

var exowebControllers = 
    angular.module('exowebControllers', ['ngRoute', 'ngGrid']);


exowebControllers.controller('ApplyCtrl', ['$scope', 'ExowebUser',
					   function($scope, ExowebUser) {
  
      var applyOkCallback = function(user) {
	  window.alert("Confirm mail sent to " + user.email);
      }
    
       var applyNokCallback = function(error) {
	   window.alert("Error: " + error);
      }
 
     $scope.apply = function(user) {
	 window.console.debug("User = " +user);
	 ExowebUser.apply(user, applyOkCallback, applyNokCallback);
     }

      $scope.passwordConfirmed = function(user) {
	  if (user.password != user.confirmpassword) {
	      window.alert("Passwords do not match! ");}
	  return angular.equals(user.password, user.confirmpassword)};

  }]);


exowebControllers.controller('ConfirmCtrl', [
    '$scope', '$routeParams', 'ExowebUser',
    function($scope, $routeParams, ExowebUser) {
      $scope.account = $routeParams.account;
      $scope.email = $routeParams.email;
      $scope.session = $routeParams.session;
      
       var okCallback = function(username) {
	   window.location =  "#/login?name="+ username;
      }
    
       var nokCallback = function(error) {
	   window.alert("Error: " + error);
      }
 
     $scope.confirm = function() {
	 var user = new Object;
	 user.email = $scope.email;
	 user.account = $scope.account;
	 user.session = $scope.session;
	 ExowebUser.confirm(user, okCallback, nokCallback);
     }
 }]);


exowebControllers.controller('LoginCtrl', ['$scope', 'ExowebUser',
  function($scope, ExowebUser) {
      
      var okCallback = function(user) {
	  ExowebUser.createCookie(user)
      }
    
      var nokCallback = function(error) {
	   window.alert("Error: " + error);
      }

      $scope.login = function(user) {
	  window.console.debug("User = " +user);
 	  ExowebUser.login(user, okCallback, nokCallback);
     }


  }]);

exowebControllers.controller('MenuCtrl', [
    '$scope', '$routeParams', 'ExowebUser',
    function($scope, $routeParams, ExowebUser) {
      
	var okCallback = function(user) {
	    $scope.user = user;
	}
	 
	 var nokCallback = function(error) {
	     window.alert("Error: " + error);
	 }

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
	 
	// If not logged in redirect
	var redirect = function() {
	    var cookie = getCookie("id");
	    window.console.debug("Cookie = " +cookie);
	    if (cookie == "") {
		window.console.debug("Empty cookie, redirecting!");
		window.location.href = "#/login";
	    }
	    else {
		ExowebUser.checkCookie(okCallback, nokCallback);
	    }
	};
	
	// Called when user clicks logout-button
	$scope.logout = function() {
	     var cookie = document.cookie;
 	     window.console.debug("Cookie before reset " + cookie);
	     document.cookie="id=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/";
	     cookie = document.cookie;
 	     window.console.debug("Cookie after reset " + cookie);
	     ExowebUser.deleteCookie(); 
	     window.location.href = "#/login";
	 };

	redirect();

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
	    if (devices.length > 0) {
		window.console.debug("devices = " + devices);
		$scope.setPageData(devices);
		$scope.selectOptions.lastPage = 
		    $scope.pagingOptions.currentPage;
		$scope.selectOptions.lastId = 
		    (devices[devices.length - 1])["id"];
		window.console.debug("Total = " + 
				     $scope.totalServerItems);
		window.console.debug("Total = " + 
				     $scope.gridOptions.totalServerItems);
		window.console.debug("Last = " + 
				     $scope.selectOptions.lastId);
		window.console.debug("Last page = " + 
				     $scope.selectOptions.lastPage);
	    }
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


	var rowSelected = function(rowItem, event) {
	    $scope.deviceid = rowItem.getProperty('id');
	    window.console.debug("Row = " +rowItem.rowIndex);
	    window.console.debug("Event = " +event);
	    window.console.debug("Id = " +$scope.deviceid);
	    DeviceDetail.getData($scope.deviceid, detailCallback);
	};

	$scope.setPageData = function(data){
	    // These variables are watched by ng-grid
	    $scope.myData = data;
	    $scope.totalServerItems = 100;
	    if (!$scope.$$phase) {
		$scope.$apply();
	    }
	};
	

	$scope.totalServerItems = 0;
	$scope.pagingOptions = {
            pageSizes: [10, 20, 50],
            pageSize: "10",
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
		window.console.debug("Size = " + $scope.pagingOptions.pageSize);
		window.console.debug("Last = " + $scope.selectOptions.lastId);
		window.console.debug("Last page = " + $scope.selectOptions.lastPage);
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

exowebControllers.controller('EditTabCtrl', ['$scope', 'Device',
    function ($scope, Device) {
	window.console.debug('Loading EditTabCtrl');
	$scope.connect = false;
	$scope.deletequeue = false;
	$scope.deletedevice = false;
	
	var updateCallback = function(device) {
	    window.alert("Device " + device.did + " updated");
	}

	$scope.update = function (device) {
	    device.did = $scope.deviceid;
	    if (device.connect == undefined) device.connect = false;
	    if (device.deletequeue == undefined) device.deletequeue = false;
	    if (device.deletedevice == undefined) device.deletedevice = false;
	    window.console.debug("Device = " +device);
	    window.console.debug("Device key = " +device.dkey);
	    window.console.debug("Server key = " +device.skey);
	    window.console.debug("MsIsdn = " +device.msisdn);
	    window.console.debug("Delete = " +device.deletedevice);
	    Device.update(device, updateCallback);
	};

	
    }
]);

exowebControllers.controller('CreateTabCtrl', ['$scope', 'Device',
    function ($scope, Device) {
	window.console.debug('Loading CreateTabCtrl');	

	var createCallback = function(device) {
	    window.alert("Device " + device.did + " created");
	}

	$scope.create = function (device) {
	    window.console.debug("Device = " +device);
	    window.console.debug("Device id = " +device.did);
	    window.console.debug("Device key = " +device.dkey);
	    window.console.debug("Server key = " +device.skey);
	    window.console.debug("MsIsdn = " +device.msisdn);
	    Device.create(device, createCallback);
	};
	

    }
]);
