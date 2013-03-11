

function navigationController($scope, $location, $routeParams) {
    $scope.hakuOid = $routeParams.hakuOid;
    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
    $scope.navClass = function (page, level) {
        var currentRoute = $location.path().split('/')[level];
        return page === currentRoute ? 'current' : '';
    };
};