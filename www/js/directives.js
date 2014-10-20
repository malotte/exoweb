'use strict';

/* Directives */

var exowebDirectives = angular.module('exowebDirectives', []);

exowebDirectives.directive('deviceTabs', function () {
    return {
        restrict: 'A',
        templateUrl: 'html/devicetabs.html',
	link: function(scope, el, attrs){
            scope.contentBaseId = attrs.tabsBaseId;

	    scope.toggleActive = function(ind){
		angular.forEach(scope.ngModel, function(value, key){
		    if (key == ind) 
			scope.ngModel[key].active = !scope.ngModel[key].active;
		    else
			scope.ngModel[key].active = false;
		});
	    }
        }
    };
});
