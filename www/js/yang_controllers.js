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
// Exoweb yang file controllers
//
// Author: Marina Westman Lönne
// Created: November 2014
//
//----------------------------------------------------------------------------

'use strict';

var exowebYangControllers = 
    angular.module('exowebYangControllers', ['ngGrid']);

var wseYangWatch = new WseWatchClass();

exowebYangControllers.controller('YangListCtrl', [
    '$scope', 'YangList', 'YangDetail',
    function($scope, YangList, YangDetail) {
	
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
	    var yangs = YangList.yangs;
	    if (yangs.length > 0) {
		window.console.debug("yangs = " + yangs);
		$scope.setPageData(yangs);
		$scope.selectOptions.lastPage = 
		    $scope.pagingOptions.currentPage;
		$scope.selectOptions.lastId = 
		    (yangs[yangs.length - 1])["filename"];
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
	    
	var rowSelected = function(rowItem, event) {
	    $scope.yangname = rowItem.getProperty('filename');
	    window.console.debug("Row = " +rowItem.rowIndex);
	    window.console.debug("Event = " +event);
	    window.console.debug("Name = " +$scope.yangname);
	    // YangDetail.getData($scope.yangname, detailCallback);
	};

	$scope.setPageData = function(data){
	    // These variables are watched by ng-grid
	    $scope.myData = data;
	    $scope.totalServerItems = 1000;
	    if (!$scope.$$phase) {
		$scope.$apply();
	    }
	};
	

	$scope.totalServerItems = 0;
	$scope.pagingOptions = {
	    totalServerItems: 0,
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
						    
	YangList.getData($scope.pagingOptions, 
			 $scope.selectOptions, 
			 $scope.filterOptions,
			 listCallback);


	
	$scope.$watch('pagingOptions', function (newVal, oldVal) {
            if (newVal !== oldVal) {
		if (newVal.pageSize !== oldVal.pageSize) {
		    newVal.currentPage = 1;
		}
		YangList.getData($scope.pagingOptions, 
				 $scope.selectOptions, 
				 $scope.filterOptions,
				 listCallback);
            }
	}, true);

	$scope.$watch('filterOptions', function (newVal, oldVal) {
            if (newVal !== oldVal) {
		YangList.getData($scope.pagingOptions, 
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
	wseYangWatch.get_data = YangList.getData;
	wseYangWatch.scope = $scope;
	wseYangWatch.callback = listCallback;

	$scope.gridOptions = {
            data: 'myData',  // Watch this variable
	    primaryKey: 'id',
 	    columnDefs: [{field:'filename', displayName:'My Files', width: 250}],
 	    headerRowHeight:0,
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



exowebYangControllers.controller('EditYangCtrl', ['$scope', 'Yang',
    function ($scope, Yang) {
	window.console.debug('Loading EditYangCtrl');

	var deleteCallback = function(yangname) {
	    window.alert("Yang " + yangname + " deleted");
	}

	$scope.remove = function (yang) {
	    Yang.remove(yang, deleteCallback);
	};

	
    }
]);

exowebYangControllers.controller('AddYangCtrl', ['$scope', 'Yang',
    function ($scope, Yang) {
	window.console.debug('Loading AddYangCtrl');

	var createCallback = function(yang) {
	    window.console.debug("Yang " + yang.filename.name + " added");
	    window.alert("Yang " + yang.filename.name + " added");
	}

	$scope.add = function () {
	    window.console.debug('Yang file = ' + 
				 JSON.stringify($scope.yang.filename));
	    Yang.create($scope.yang, createCallback);
	};

 
    }
]);
