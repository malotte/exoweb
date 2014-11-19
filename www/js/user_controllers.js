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
		angular.forEach($scope.myUsers, function(data, index){
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
	    $scope.user = UserDetail.user;
	    window.console.debug("User details = " + 
				 JSON.stringify($scope.user));
	    $scope.$apply();
	}


	var rowSelected = function(rowItem, event) {
	    var username = rowItem.getProperty('name');
	    window.console.debug("Row = " +rowItem.rowIndex);
	    window.console.debug("Event = " +event);
	    window.console.debug("Name = " +username);
	    UserDetail.getData(username, detailCallback);
	};

	$scope.setPageData = function(data){
	    // These variables are watched by ng-grid
	    $scope.myUsers = data;
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
            if (newVal !== oldVal) {
		if (newVal.pageSize !== oldVal.pageSize) {
		    newVal.currentPage = 1;
		}
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
            data: 'myUsers',  // Watch this variable
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

	var updateCallback = function(user) {
	    if (user.deleteuser == true)
		window.alert("User " + user.name + " deleted");
	    else
		window.alert("User " + user.name + " updated");
	    user.password = undefined;
	    user.confirmpassword = undefined;
	    $scope.$digest();
	}

	$scope.update = function (user) {
	    if (user.deleteuser == undefined) user.deleteuser = false;
	    if (user.phone == undefined) user.phone = "";
	    window.console.debug("User = " +JSON.stringify(user));
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

	var createCallback = function(user) {
	    window.alert("User " + user.name + " created");
	}

	$scope.add = function (user) {
	    if (user.phone == undefined) user.phone = "";
	    window.console.debug("User to add = " +JSON.stringify(user));
	    User.create(user, createCallback);
	};

      $scope.passwordConfirmed = function(user) {
	  if (user.password != user.confirmpassword) {
	      window.alert("Passwords do not match! ");}
	  return angular.equals(user.password, user.confirmpassword)};
	

    }
]);
