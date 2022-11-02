/// <reference types="cypress" />


describe('Render tests', () => {
    const haskellcode = "a = maximum [3,2,6,4,1,2,3]"

    


    beforeEach(() => {
      cy.visit('http://localhost:3000/')
    })

    it('renders home page', () => {})

    it('Can write and send test and recieve result.', () => {
      cy.get('[data-cy="code-text-input-file"]').click().type(haskellcode)

      cy.get('[data-cy="submit-code"]').click()
    })
})