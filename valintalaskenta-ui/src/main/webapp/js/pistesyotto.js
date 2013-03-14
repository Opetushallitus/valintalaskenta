function PistesyottoController($scope, $location, $routeParams, HakukohdeModel) {
    $scope.hakukohdeModel = HakukohdeModel;
    $scope.hakukohdeModel.refreshIfNeeded($routeParams.hakukohdeOid);
}
