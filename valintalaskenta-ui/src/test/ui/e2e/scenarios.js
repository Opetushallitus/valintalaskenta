'use strict';

describe('E2E-TESTS', function() {
    describe('valintalaskenta', function() {
    
        beforeEach(function() {
            browser().navigateTo('../../html/index-e2e.html');
        });


        it('should redirect to rootpage', function() {
            browser().navigateTo('../../html/index-e2e.html');
            expect(browser().location().url()).toBe('/haku/oid1/hakukohde/');
        });

        it('should redirect to haku', function() {
            select("model.hakuOid").option(1)
            expect(browser().location().url()).toBe('/haku/oid2/hakukohde/');
            element('.selectlist a :last').click();
            expect(browser().location().url()).toBe('/haku/oid2/hakukohde/oid4/valinnanhallinta');
            element('.hakukohdeNav a :last').click();
            expect(browser().location().url()).toBe('/haku/oid2/hakukohde/oid4/valintaesitys');
        });

        it('should redirect to hakukohde', function() {
            browser().navigateTo('../../html/index-e2e.html#/haku/oid2/hakukohde/oid1/valintaesitys');
            expect(browser().location().url()).toBe('/haku/oid2/hakukohde/oid1/valintaesitys');
            expect(repeater('.grid16-12 .tabsheets div.ng-scope').count()).toEqual(3);
        });

    });
});




