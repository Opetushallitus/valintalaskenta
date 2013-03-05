'use strict';

describe('E2E-TESTS', function() {
  describe('valintalaskenta', function() {
    
    beforeEach(function() {
      browser().navigateTo('../../html/index.html');
    });


    it('should redirect to rootpage', function() {
      browser().navigateTo('../../index.html');
      pause();
      expect(browser().location().url()).toBe('/haku/oid1/hakukohde/');
      pause();
    });

  });  
});

