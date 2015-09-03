angular.module('whatIfCSH', [])
.controller('WhatIfController', function($scope, $http) {
    var base = 'https://whatif.csh.rit.edu/api'
    $scope.suggestions = [];
    $scope.description = "";
    $scope.error = null;

    var errorCallback = function(_, _, _, _) {
        $scope.error = "Something went wrong.";
    }

    $http.get(base + '/suggestions')
        .success(function(data, status, headers, config) {
            data.suggestions.forEach(function(suggestion, _, _) {
                if (suggestion.vote) {
                    console.log(suggestion.score);
                    suggestion.score -= suggestion.vote;
                }
            });
            $scope.suggestions = data.suggestions;
            $scope.user = data.user;
        })
    .error(errorCallback);

    $scope.score = function(suggestion) {
        var score = suggestion.score + (suggestion.vote == null ? 0 : suggestion.vote)
        return score + " point" + (score == 1 ? '' : 's');
    }

    $scope.vote = function(suggestion, upvote) {
        if ((suggestion.vote == 1 && upvote) || (suggestion.vote == -1 && !upvote)) {
            $http.delete(base + '/suggestions/' + suggestion.id + '/vote')
                .success(function(data, status, headers, config) {
                    suggestion.vote = null;
                });
            return;
        }
        $http.put(base + '/suggestions/' + suggestion.id + '/vote/' + (upvote ? 'up' : 'down'))
            .success(function(data, status, headers, config) {
                suggestion.vote = upvote ? 1 : -1;
            });
    };

    $scope.upvoteClass = function(suggestion) {
        var cls = 'vote-unclicked';
        if (suggestion && suggestion.vote == 1) {
            cls = 'vote-clicked';
        }
        return cls + ' unstyled-button'
    };

    $scope.downvoteClass = function(suggestion) {
        var cls = 'vote-unclicked';
        if (suggestion && suggestion.vote == -1) {
            cls = 'vote-clicked';
        }
        return cls + ' unstyled-button'
    };

    $scope.submit = function() {
        var trimmed = $scope.description.trim();
        if (!trimmed) return;
        $http.post(base + '/suggestions', {description: trimmed})
            .success(function(data, status, headers, config) {
                $scope.description = "";
                $scope.suggestions.splice(0, 0, data);
            })
        .error(errorCallback);
    };

    $scope.delete = function(suggestion) {
        $http.delete(base + "/suggestions/" + suggestion.id)
            .success(function(data, status, headers, config) {
                var index = $scope.suggestions.indexOf(suggestion);
                if (index > -1) {
                    $scope.suggestions.splice(index, 1);
                }
            })
        .error(errorCallback);
    }
    $scope.timestamp = function(suggestion) {
        var m = moment(suggestion.timestamp);
        return m.fromNow()
    }
});
