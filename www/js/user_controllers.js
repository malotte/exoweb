'use strict';

/* Controllers */

var exowebUserControllers = 
    angular.module('exowebUserControllers', ['ngGrid']);

exowebUserControllers.controller('UserListCtrl', [
    '$scope', 'UserList', 'UserDetail',
    function($scope, UserList, UserDetail) {
	
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
	    var users = UserList.users;
	    if (users.length > 0) {
		window.console.debug("users = " + users);
		$scope.setPageData(users);
		$scope.selectOptions.lastPage = 
		    $scope.pagingOptions.currentPage;
		$scope.selectOptions.lastId = 
		    (users[users.length - 1])["id"];
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
	    var user = UserDetail.user;
	    window.console.debug("user = " + user);
	    $scope.userphone = user.phone;
	    $scope.useremail = user.email;
	    $scope.userrole = user.role;
	    $scope.$apply();
	}


	var rowSelected = function(rowItem, event) {
	    $scope.username = rowItem.getProperty('name');
	    window.console.debug("Row = " +rowItem.rowIndex);
	    window.console.debug("Event = " +event);
	    window.console.debug("Name = " +$scope.username);
	    UserDetail.getData($scope.username, detailCallback);
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
						    
	UserList.getData($scope.pagingOptions, 
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
		UserList.getData($scope.pagingOptions, 
				   $scope.selectOptions, 
				   $scope.filterOptions,
				   listCallback);
            }
	}, true);

	$scope.$watch('filterOptions', function (newVal, oldVal) {
            if (newVal !== oldVal) {
		UserList.getData($scope.pagingOptions, 
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
 	    columnDefs: [{field:'name', displayName:'Users', width: 200}, 
			 {field:'role', displayName:'Role', width: 100}],
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


exowebUserControllers.controller('ReadUserCtrl', ['$scope', 
    function ($scope) {
	window.console.debug('Loading ReadUserCtrl');
	$scope.title = 'Read';
    }
]);

exowebUserControllers.controller('EditUserCtrl', ['$scope', 'User',
    function ($scope, User) {
	window.console.debug('Loading EditUserCtrl');
	$scope.roles = ["view", "config", "execute", "admin"];
	$scope.deleteuser = false;
	
	var updateCallback = function(user) {
	    window.alert("User " + user.name + " updated");
	}

	$scope.update = function (user) {
	    user.name = $scope.username;
	    if (user.deleteuser == undefined) user.deleteuser = false;
	    window.console.debug("User = " +user);
	    window.console.debug("Phone = " +user.phone);
	    window.console.debug("Email = " +user.email);
	    window.console.debug("Role = " +user.role);
	    window.console.debug("Delete = " +user.deleteuser);
	    User.update(user, updateCallback);
	};

	$scope.passwordConfirmed = function(user) {
	    if (user.password != user.confirmpassword) {
		window.alert("Passwords do not match! ");}
	    return angular.equals(user.password, user.confirmpassword)};
	
    }
]);

exowebUserControllers.controller('AddUserCtrl', ['$scope', 'User',
    function ($scope, User) {
	window.console.debug('Loading AddUserCtrl');

	$scope.roles = ["view", "config", "execute", "admin"];
	$scope.role = "view";

	var createCallback = function(user) {
	    window.alert("User " + user.name + " created");
	}

	$scope.add = function (user) {
	    window.console.debug("User = " +user);
	    window.console.debug("Phone = " +user.phone);
	    window.console.debug("Email = " +user.email);
	    window.console.debug("Role = " +user.role);
	    User.create(user, createCallback);
	};

      $scope.passwordConfirmed = function(user) {
	  if (user.password != user.confirmpassword) {
	      window.alert("Passwords do not match! ");}
	  return angular.equals(user.password, user.confirmpassword)};
	

    }
]);
