﻿function HarkinnassaController($scope, $location, $routeParams, HakukohdeModel) {
    $scope.hakukohdeModel = HakukohdeModel;
    HakukohdeModel.refreshIfNeeded($routeParams.hakukohdeOid);
}