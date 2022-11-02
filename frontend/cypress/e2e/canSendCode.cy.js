import './datatags/home-site'
import { text_field, submit, result_tab } from './datatags/home-site'
/// <reference types="cypress" />

describe('Render tests', () => {
    const correct_code = "a = maximum [3,2,6,4,1,2,3]"
    


    beforeEach(() => {
      cy.visit('http://localhost:3000/')
    })

    it('renders home page', () => {})

    it('Can write and send test and recieve result.', () => {
      cy.get(text_field).type(correct_code)
      cy.get(submit).click()
      cy.get(result_tab).should('be.enabled')
    })
})