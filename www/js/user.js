'use strict';

/* Controllers */

var userMod = angular.module('userMod', ['ngCookies']);

userMod.controller('UserListCtrl', ['$scope', 'User',
  function($scope, User) {
    $scope.users = User.query();
    $scope.orderProp = 'name';
  }]);

