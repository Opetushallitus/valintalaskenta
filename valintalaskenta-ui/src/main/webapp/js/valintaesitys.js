
app.factory('ValintaesitysModel', function(Hakukohde, Valintatapajono, Valinnanvaihe) {

	var model = new function() {
		/*
		Hakukohde.get({hakukohdeoid: "hakukohdeoid-isvjun"}, function(result) {
			console.log(result);
		});
		
		Valintatapajono.get({valintatapajonoid: "jonooid-fdgajq"}, function(result) {
			console.log(result);
		});

		Valinnanvaihe.get({valinnanvaiheoid: "valinnanvaiheoid-zguzjf"}, function(result) {
			console.log(result);
		});
	*/

	};

	return model;
});


function ValintaesitysController($scope, $location, $routeParams, ValintaesitysModel) {
    $scope.hakuOid = $routeParams.hakuOid;
    $scope.model = ValintaesitysModel;
    $scope.hakukohdeOid = $routeParams.hakukohdeOid;
}