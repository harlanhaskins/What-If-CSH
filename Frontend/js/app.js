angular.module('whatIfCSH', [])
  .controller('WhatIfController', function($scope, $http) {
    var base = 'http://church.csh.rit.edu:5777';
    $scope.suggestions = [];
    $http.get(base + '/suggestions')
         .success(function(data, status, headers, config) {
             $scope.suggestions = $scope.suggestions.concat(data);
         });
    $scope.submit = function() {
        if (!$scope.description) return;
        console.log($scope.description);
        var trimmed = $scope.description.trim();
        if (!trimmed) {
            return;
        }
        $http.post(base + '/suggestions', {description: trimmed})
             .success(function(data, status, headers, config) {
               $scope.description = "";
               $scope.suggestions.push(data);
             })
             .error(function(data, status, headers, config) {
               console.log(data);
               console.log(status);
               console.log(config);
             })
    };
});
