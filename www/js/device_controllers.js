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
// Exoweb device controllers
//
// Author: Marina Westman Lönne
// Created: October 2014
//
//----------------------------------------------------------------------------

'use strict';

var wseDeviceNotify = new WseNotifyClass();

/* Controllers */

var exowebDeviceControllers = 
    angular.module('exowebDeviceControllers', ['ngGrid']);

var exodmSession;

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
	    // A lot of code to handle that we actually don't know
	    // total number of devices
	    if (devices.length < $scope.pagingOptions.pageSize) {
		// Last page, don't go further
		window.console.debug("devices = " + JSON.stringify(devices));
		if ($scope.pagingOptions.currentPage >= 
		    $scope.selectOptions.lastPage) {
		    // Going forwards
		    $scope.setPageData(devices);
		    $scope.selectOptions.lastPage = 
			$scope.pagingOptions.currentPage - 1;
		    $scope.selectOptions.lastId = "";
		}
		else {
		    // Going backwards, 
		    
		    $scope.setPageData(devices);
		    $scope.selectOptions.lastPage = 
			$scope.pagingOptions.currentPage + 1;
		    $scope.selectOptions.lastId = "";
		}
	    }
	    else if (devices.length > 0) {
		// Normal case
		window.console.debug("devices = " + JSON.stringify(devices));
		$scope.setPageData(devices);
		$scope.selectOptions.lastPage = 
		    $scope.pagingOptions.currentPage;
		$scope.selectOptions.lastId = 
		    (devices[devices.length - 1])["id"];
	    }
	    else if ($scope.pagingOptions.currentPage !== 1){
		// Border case 
		if ($scope.pagingOptions.currentPage >= 
		    $scope.selectOptions.lastPage) {
		    // Going forwards, fetch last  page
		    $scope.selectOptions.lastId = "";
		    DeviceList.getData($scope.pagingOptions, 
				       $scope.selectOptions, 
				       $scope.filterOptions,
				       listCallback);
		}
		else {
		    // Going backward, get last page
		    $scope.selectOptions.lastPage = 
			$scope.pagingOptions.currentPage - 1; 
		    $scope.selectOptions.lastId = "";
		    DeviceList.getData($scope.pagingOptions, 
				       $scope.selectOptions, 
				       $scope.filterOptions,
				       listCallback);
		}
	    }
	    else {
		// No devices at all, do nothing
	    }
	    if ($scope.selectedItem !== undefined)
		$scope.gridOptions.selectItem($scope.selectedItem, true);
	    window.console.debug("Total = " + 
				     $scope.totalItems);
	    window.console.debug("Total = " + 
				     $scope.gridOptions.totalServerItems);
	    window.console.debug("Last = " + 
				     $scope.selectOptions.lastId);
	    window.console.debug("Last page = " + 
				     $scope.selectOptions.lastPage);
	    window.console.debug("Current page = " + 
				     $scope.pagingOptions.currentPage);
	};
	    
	var detailCallback = function() {
	    $scope.device = DeviceDetail.device;
	    window.console.debug("Device details = " + 
				 JSON.stringify($scope.device));
	    
	    if ($scope.editMode == true && 
		$scope.selectedLocked == false)
		$scope.buttonsEnabled = true;
	    else
		$scope.buttonsEnabled = false;

	    $scope.$apply();
	}


	var rowSelected = function(rowItem, event) {
	    var deviceid = rowItem.getProperty('id');
	    $scope.selectedLocked = rowItem.getProperty('locked')
	    $scope.selectedRowIndex = rowItem.rowIndex;
	    $scope.selectedItem = deviceid;
	    if ($scope.reservedItem !== undefined) {
		// Release previous selection in exodm
		DeviceDetail.release($scope.reservedItem);
		$scope.reservedItem = undefined;
	    }
	    if ($scope.editMode == true && $scope.selectedLocked == false) {
		// Reserve this selection in exodm
		DeviceDetail.reserve(deviceid);
		$scope.reservedItem = $scope.selectedItem;
	    }
	    window.console.debug("Row = " + rowItem.rowIndex);
	    window.console.debug("Event = " + event);
	    window.console.debug("Id = " + deviceid);
	    DeviceDetail.getData(deviceid, detailCallback);
	};

	$scope.setPageData = function(data){
	    // These variables are watched by ng-grid
	    $scope.myDevices = data;
	    $scope.totalItems = data.length; // Large number ???
	    if (!$scope.$$phase) $scope.$apply();
	}

	// Edit mode switch
	$scope.editModeChange = function(edit) {
	    $scope.editMode = edit.mode;
	    window.console.debug("Edit mode = " + edit.mode);
	    window.console.debug("Edit mode = " + $scope.editMode);
	    window.console.debug("Selected locked = " + $scope.selectedLocked);
	    window.console.debug("Buttons enabled = " + $scope.buttonsEnabled);
	    if ($scope.editMode == true && 
		$scope.selectedLocked == false) {
		$scope.buttonsEnabled = true;
		// Reserve current selection in exodm
		if ($scope.selectedItem !== undefined) {
		    DeviceDetail.reserve($scope.selectedItem);
		    $scope.reservedItem = $scope.selectedItem;
		}
	    }
	    else {
		$scope.buttonsEnabled = false;
		if ($scope.reservedItem !== undefined) {
		    // Release current selection in exodm
		    DeviceDetail.release($scope.reservedItem);
		    $scope.reservedItem = undefined;
		}
	    }
	}
	$scope.editMode = false;
	$scope.selectedLocked = true;
	$scope.buttonsEnabled = false;

	// Session id to use when reserving in exodm
	exodmSession = Math.floor((Math.random() * 100000) + 1);	

	$scope.totalItems = 0;
	$scope.pagingOptions = {
	    totalServerItems: 100000, // A large number since we don't know
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
	
	// Needed when change notification comes from exodm
	wseDeviceNotify.get_data = DeviceList.getData;
	wseDeviceNotify.scope =  $scope;
	wseDeviceNotify.callback = listCallback;

	$scope.gridOptions = {
            data: 'myDevices',  // Watch this variable
	    primaryKey: 'id',
 	    columnDefs: [{field:'id', width: 100}, 
			 {field:'status', width: 100},
			 {field:'created', width: 90},
			 {field:'changed', width: 90},
			 {field:'inqueue', width: 80},
			 {field:'locked', width: 40, 
			  cellTemplate : 'html/lockIcon.html'}],
	    headerRowHeight:0,
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
	    window.alert("Device " + device.did + " updated");
	}

	var deleteCallback = function(device) {
	    window.alert("Device " + device.did + " deleted");
	    $scope.gridOptions.selectRow($scope.selectedRowIndex + 1, true);
	}

	$scope.connect = function (device) {
	    window.console.debug("Device to connect = " + JSON.stringify(device));
	    window.alert("Not implemented yet!");
	};

	$scope.deletequeue = function (device) {
	    window.console.debug("Device to delete queue for = " + 
				 JSON.stringify(device));
	    window.alert("Not implemented yet!");
	};

	$scope.update = function (device) {
	    window.console.debug("Device to update = " + JSON.stringify(device));
	    if (device.did == undefined)
		window.alert("No device selected!")
	    else 
		Device.update(device, updateCallback);
	};

	$scope.remove = function (device) {
	    window.console.debug("Device to delete = " + JSON.stringify(device));
	     if (device.did == undefined)
		window.alert("No device selected!")
	    else 
		Device.remove(device, deleteCallback);
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
	    if (device == undefined)
		window.alert("No device specified!")
	    else if (device.did == undefined)
		window.alert("No device id specified!")
	    else
		Device.create(device, createCallback);
	};
	

    }
]);
