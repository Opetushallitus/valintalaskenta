//domain .. this is both, service & domain layer
app.factory('Treemodel', function($resource, RootValintaryhmas, ChildValintaryhmas, RootHakukohde, ChildHakukohdes) {
    //keep model to your self
    var model = [ ];
    //and return interface for manipulating the model
    var modelInterface =  {
        getValintaryhmaTree:function()  {
            return model.valintaryhma;
        },
        getRootHakukohde:function() {
             return model.hakukohde;
        },
        expandNode:function(node) {

               if( node.lapsivalintaryhma) {
                      ChildValintaryhmas.get({parentOid: node.oid}, function(result) {
                             node.lapsivalintaryhmaList = result;
                       });
               }
               if(node.lapsihakukohde) {
                     ChildHakukohdes.get({oid: node.oid}, function(result) {
                         node.lapsihakukohdeList = result;
                      });
               }



        },
        refresh:function() {
               //get initial listing
               RootValintaryhmas.get({},function(result) {
                     model.valintaryhma = result;
               });
               RootHakukohde.get({},function(result) {
                      model.hakukohde = result;
               });
        }
    };
    modelInterface.refresh();
  return modelInterface;
});


function ValintaryhmaHakukohdeTreeController($scope, $location, Treemodel) {
       $scope.domain = Treemodel;
       $scope.expandNode = function(node) {
            if( (node.lapsivalintaryhma && (node.lapsivalintaryhmaList == null || node.lapsivalintaryhmaList.length <= 0) )  ||
                (node.lapsihakukohde && (node.lapsihakukohdeList == null || node.lapsihakukohdeList.length <= 0 ) )
              ) {
              Treemodel.expandNode(node);
              node.isVisible = true;
            }  else if(node.isVisible != true) {
                 node.isVisible = true;
            } else {
                  node.isVisible = false;
            }
        }

}