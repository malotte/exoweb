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
// Exoweb application module
//
// Author: Marina Westman Lönne
// Created: September 2014
//
//----------------------------------------------------------------------------

'use strict';

var exowebApp = angular.module('exoweb', [
    'ngRoute',
    'toggle-switch',
    'exowebControllers',
    'exowebDeviceControllers',
    'exowebUserControllers',
    'exowebYangControllers',
    'exowebDirectives',
    'exowebServices',
    'exowebDeviceServices',
    'exowebUserServices',
    'exowebYangServices'
]);

exowebApp.config(['$routeProvider', '$locationProvider',
		  function($routeProvider, $locationProvider) 
{
		  
    $routeProvider.
	when('/signup', {
            templateUrl: 'html/signup.html'
	}).
	when('/confirm?account&email&session', {
            templateUrl: 'html/confirm.html'
	}).
	when('/confirm', {
            templateUrl: 'html/confirm.html'
	}).
	when('/login?name', {
            templateUrl: 'html/login.html'
	}).
	when('/login', {
            templateUrl: 'html/login.html'
	}).
	when('/device', {
            templateUrl: 'html/device.html'
	}).
	when('/user', {
            templateUrl: 'html/user.html'
	}).
	when('/yang', {
            templateUrl: 'html/yang.html'
	}).
	when('/howto', {
            templateUrl: 'html/howto.html'
	}).
	otherwise({
            redirectTo: '/login'
	});
 
    // $locationProvider.html5Mode(true);
  }]);
