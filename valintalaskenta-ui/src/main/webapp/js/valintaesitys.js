
app.factory('ValintaesitysModel', function(Haku) {

	var model = new function() {

		Haku.get({}, function(result) {
			console.log(result);
		});

	};

	return model;
});


function ValintaesitysController($scope, $location, $routeParams, ValintaesitysModel) {
    $scope.hakuOid = $routeParams.hakuOid;
    $scope.model = ValintaesitysModel;
    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
}