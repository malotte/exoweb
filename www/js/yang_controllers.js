'use strict';

/* Controllers */

var exowebYangControllers = 
    angular.module('exowebYangControllers', ['ngGrid']);

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
		    (yangs[yangs.length - 1])["id"];
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
	    var yang = YangDetail.yang;
	    window.console.debug("yang = " + yang);
	    $scope.$apply();
	}


	var rowSelected = function(rowItem, event) {
	    $scope.yangname = rowItem.getProperty('name');
	    window.console.debug("Row = " +rowItem.rowIndex);
	    window.console.debug("Event = " +event);
	    window.console.debug("Name = " +$scope.yangname);
	    YangDetail.getData($scope.yangname, detailCallback);
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
						    
	YangList.getData($scope.pagingOptions, 
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
	
	$scope.gridOptions = {
            data: 'myData',  // Watch this variable
	    primaryKey: 'id',
 	    columnDefs: [{field:'filename', displayName:'My Files', width: 200}],
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
	$scope.roles = ["view", "config", "execute", "admin"];
	$scope.deleteyang = false;
	//$scope.yang.role = $scope.yangrole;

	var updateCallback = function(yang) {
	    window.alert("Yang " + yang.name + " updated");
	}

	$scope.update = function (yang) {
	    window.console.debug("Update " + $scope.yangname + " pressed.");
	    yang.name = $scope.yangname;
	    if (yang.deleteyang == undefined) yang.deleteyang = false;
	    window.console.debug("Delete = " +yang.deleteyang);
	    Yang.update(yang, updateCallback);
	};

	
    }
]);

exowebYangControllers.controller('AddYangCtrl', ['$scope', 'Yang',
    function ($scope, Yang) {
	window.console.debug('Loading AddYangCtrl');

	var createCallback = function(yang) {
	    window.alert("Yang " + yang.filename + " created");
	}

	$scope.add = function () {
	    var yang = new Object;
	    yang.filename = $scope.yang.filename;
	    window.console.debug('Yang file = ' + JSON.stringify(yang.filename));
	    Yang.create(yang, createCallback);
	};

 
    }
]);
