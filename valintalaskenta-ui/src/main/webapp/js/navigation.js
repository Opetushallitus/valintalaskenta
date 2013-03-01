app.controller('navigationController', ['$scope', '$location', function ($scope, $location) {
    $scope.navClass = function (page, level) {
        var currentRoute = $location.path().split('/')[level];
        return page === currentRoute ? 'current' : '';
    };
}]);