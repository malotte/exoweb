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
// Exoweb user controllers
//
// Author: Marina Westman Lönne
// Created: October 2014
//
//----------------------------------------------------------------------------

'use strict';

var exowebUserControllers = 
    angular.module('exowebUserControllers', ['ngGrid', 'ngDialog']);

var wseUserWatch = new WseWatchClass();

exowebUserControllers.controller('UserListCtrl', [
    '$scope', 'UserList', 'UserDetail',
    function($scope, UserList, UserDetail) {
	$scope.myModel = {myUsers:[],
			  totalItems:0};

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
		window.console.debug("users = " + JSON.stringify(users));
		$scope.setPageData(users);
		$scope.selectOptions.lastPage = 
		    $scope.pagingOptions.currentPage;
		$scope.selectOptions.lastId = 
		    (users[users.length - 1])["name"];
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
	    window.console.debug("set data " + JSON.stringify(data));
	    // These variables are watched by ng-grid
	    $scope.myModel.myUsers = data;
	    $scope.myModel.totalItems = data.length; 
	    $scope.gridOptions.totalServerItems = data.length;
	    if (!$scope.$$phase) {
		window.console.debug("set data apply" + 
				     JSON.stringify($scope.myUsers));
		$scope.$apply();
	    }
	};
	

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
	
	// Needed when change notification comes from exodm
	wseUserWatch.get_data = UserList.getData;
	wseUserWatch.scope =  $scope;
	wseUserWatch.callback = listCallback;

	$scope.gridOptions = {
            data: 'myModel.myUsers',  // Watch this variable
	    primaryKey: 'id',
 	    columnDefs: [{field:'name', displayName:'Users', width: 150}, 
			 {field:'role', displayName:'Role', width: 150}],
	    headerRowHeight:0,
            totalServerItems: 'myModel.totalItems', // Watch this variable
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

exowebUserControllers.controller('EditUserCtrl', ['$scope', 'User', 'ngDialog',
    function ($scope, User, ngDialog) {
	window.console.debug('Loading EditUserCtrl');
	$scope.filter = {
	    roles: [{name: "view", selectable: true}, 
		    {name: "config", selectable: true}, 
		    {name: "execute", selectable: true}, 
		    {name: "admin", selectable: true},
		    {name: "initial-admin", selectable: false}]};

	var updateCallback = function(user) {
	    window.alert("User " + user.name + " updated");
	}

	var deleteCallback = function(user) {
	    window.alert("User " + user.name + " deleted");
	}

	$scope.update = function (user) {
	    if (user.phone == undefined) user.phone = "";
	    window.console.debug("User = " +JSON.stringify(user));
	    if (user.name == undefined) 
		window.alert("No user selected!");
	    else 
		User.update(user, updateCallback);
	};

	$scope.remove = function (user) {
	    window.console.debug("User = " +JSON.stringify(user));
	    if (user.name == undefined) 
		window.alert("No user selected!");
	    else 
		User.remove(user, deleteCallback);
	};

	$scope.open = function () {
	    ngDialog.open({
		templateUrl: 'html/password.html',
		scope: $scope
	    });
	};
	
    }
]);

exowebUserControllers.controller('UserPassCtrl', ['$scope', 'User', 'ngDialog',
    function ($scope, User, ngDialog) {
	window.console.debug('Loading UserPassCtrl');

	var updateCallback = function(user) {
	    window.alert("User " + user.name + " updated");
	    user.password = undefined;
	    user.confirmpassword = undefined;
	    ngDialog.close();
	}

	$scope.changepassword = function (user) {
	    window.console.debug("User = " +JSON.stringify(user));
	    User.changepassword(user, updateCallback);
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
	    if (user.role == "initial-admin")
		// This is checked in exodm as well
		window.alert("Not possible to create an initial-admin! ");
	    else if (user == undefined)
		window.alert("No user specified!");
	    else if (user.name == undefined)
		window.alert("No user name specified!");
	    else
		User.create(user, createCallback);
	};

      $scope.passwordConfirmed = function(user) {
	  if (user.password != user.confirmpassword) {
	      window.alert("Passwords do not match! ");}
	  return angular.equals(user.password, user.confirmpassword)};
	

    }
]);
