
app.factory('HakemusModel', function(Hakemus, KaikkiHakemukset) {

	var model = new function() {

		this.hakemukset = [];


		//Haetaan toistaiseksi kaikki tarjolla olevat hakemukset
		this.refresh = function() {
			KaikkiHakemukset.get({}, function(result) {
				model.hakemukset = result;
				console.log(model.hakemukset);
			});
		};
	};

	return model;

});

function PistesyottoController($scope, $location, $routeParams, HakukohdeModel, HakemusModel) {
    $scope.model = HakemusModel;
    $scope.model.refresh();

    $scope.hakukohdeModel = HakukohdeModel;
    $scope.hakukohdeModel.refreshIfNeeded($routeParams.hakukohdeOid);
}

