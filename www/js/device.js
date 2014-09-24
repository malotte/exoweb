'use strict';

/* Controllers */

var deviceMod = angular.module('deviceMod', ['ngCookies']);

deviceMod.controller('DeviceListCtrl', ['$scope', 'Device',
  function($scope, Device) {
    $scope.devices = Device.query();
    $scope.orderProp = 'age';
  }]);

deviceMod.controller('DeviceDetailCtrl', ['$scope', '$routeParams', 'Device',
  function($scope, $routeParams, Device) {
    $scope.device = 
	  Device.get({deviceId: $routeParams.deviceId}, 
		     function(device) {
			 $scope.mainImageUrl = device.images[0];
		     });

      $scope.setImage = function(imageUrl) {
	  $scope.mainImageUrl = imageUrl;
      }
  }]);

