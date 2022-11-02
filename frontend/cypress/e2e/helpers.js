/// <reference types="cypress" />

export const get_and_click = (element_tag) => {
  let elem = cy.get(text_field).click()
  return elem
} 