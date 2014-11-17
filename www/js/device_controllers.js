'use strict';

/* Controllers */

var exowebDeviceControllers = 
    angular.module('exowebDeviceControllers', ['ngGrid']);

exowebDeviceControllers.controller('DeviceListCtrl', [
    '$scope', 'DeviceList', 'DeviceDetail',
    function($scope, DeviceList, DeviceDetail) {
	
	var scroll = function(rowItem, event){
           if(!event.ctrlKey && !event.shiftKey && event.type != 'click'){
		var grid = $scope.gridOptions.ngGrid;
		grid.$viewport.scrollTop(rowItem.offsetTop - (grid.config.rowHeight * 2));
		angular.forEach($scope.myDevices, function(data, index){
		    $scope.gridOptions.selectRow(index, false);
		});
            }
            return true;
	}
	
	var listCallback = function() {
	    var devices = DeviceList.devices;
	    if (devices.length > 0) {
		window.console.debug("devices = " + JSON.stringify(devices));
		$scope.setPageData(devices);
		$scope.selectOptions.lastPage = 
		    $scope.pagingOptions.currentPage;
		$scope.selectOptions.lastId = 
		    (devices[devices.length - 1])["id"];
		window.console.debug("Total = " + 
				     $scope.totalItems);
		window.console.debug("Total = " + 
				     $scope.gridOptions.totalServerItems);
		window.console.debug("Last = " + 
				     $scope.selectOptions.lastId);
		window.console.debug("Last page = " + 
				     $scope.selectOptions.lastPage);
	    }
	};
	    
	var detailCallback = function() {
	    $scope.device = DeviceDetail.device;
	    window.console.debug("Device details = " + 
				 JSON.stringify($scope.device));
	    $scope.$apply();
	}


	var rowSelected = function(rowItem, event) {
	    var deviceid = rowItem.getProperty('id');
	    window.console.debug("Row = " +rowItem.rowIndex);
	    window.console.debug("Event = " +event);
	    window.console.debug("Id = " +$scope.deviceid);
	    DeviceDetail.getData(deviceid, detailCallback);
	};

	$scope.setPageData = function(data){
	    // These variables are watched by ng-grid
	    $scope.myDevices = data;
	    $scope.totalItems = data.length; // Large number ???
	    if (!$scope.$$phase) {
		$scope.$apply();
	    }
	};
	

	$scope.totalItems = 0;
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
            if (newVal !== oldVal) {
		if (newVal.pageSize !== oldVal.pageSize) {
		    newVal.currentPage = 1;
		}
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
            data: 'myDevices',  // Watch this variable
	    primaryKey: 'id',
 	    columnDefs: [{field:'id', displayName:'My devices', width: 100}, 
			 {field:'status', displayName:'Status', width: 100}],
            totalServerItems: 'totalItems', // Watch this variable
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


exowebDeviceControllers.controller('ReadDeviceCtrl', ['$scope', 
    function ($scope) {
	window.console.debug('Loading ReadDeviceCtrl');
	$scope.title = 'Read';
    }
]);

exowebDeviceControllers.controller('EditDeviceCtrl', ['$scope', 'Device',
    function ($scope, Device) {
	window.console.debug('Loading EditDeviceCtrl');
	
	var updateCallback = function(device) {
	    if (device.deletedevice == true)
		window.alert("Device " + device.did + " deleted");
	    else
		window.alert("Device " + device.did + " updated");
	}

	$scope.update = function (device) {
	    if (device.connect == undefined) device.connect = false;
	    if (device.deletequeue == undefined) device.deletequeue = false;
	    if (device.deletedevice == undefined) device.deletedevice = false;
	    window.console.debug("Device to change = " + JSON.stringify(device));
	    Device.update(device, updateCallback);
	};

	
    }
]);

exowebDeviceControllers.controller('AddDeviceCtrl', ['$scope', 'Device',
    function ($scope, Device) {
	window.console.debug('Loading AddDeviceCtrl');	

	var createCallback = function(device) {
	    window.console.debug("Device = " + JSON.stringify(device) + " created.");
	    window.alert("Device " + device.did + " created.");
	}

	$scope.add = function (device) {
	    window.console.debug("Device to add = " + JSON.stringify(device));
	    Device.create(device, createCallback);
	};
	

    }
]);
