app.controller('navigationController', ['$scope', '$location', function ($scope, $location) {
    $scope.navClass = function (page, level) {
        console.log($location.path().split('/'));
        var currentRoute = $location.path().split('/')[level];
        console.log('page: ' + page + ' currentRoute: ' + currentRoute);
        return page === currentRoute ? 'current' : '';
    };
}]);